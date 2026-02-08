#' Import Final Source Selection into DR4 Operations Table
#'
#' Imports a final run selection for a specific SOS module into the
#' \code{dr4_ops_cs48_mv.dr4_final_run_selection} table. Supports two
#' import methods: CSV-based ingestion from the Gaia OwnCloud WebDAV
#' repository, or direct SQL insertion from a subquery.
#'
#' The function first deletes any existing rows for the specified module
#' to ensure idempotent imports, then inserts new data via the chosen method.
#'
#' @param con A DBI database connection object to the surveys database.
#'   Must have write access to \code{dr4_ops_cs48_mv.dr4_final_run_selection}.
#' @param sosSubName Character string identifying the SOS sub-module name
#'   (e.g., \code{"RR_LYRAE"}, \code{"CEPHEID"}). Used as:
#'   \itemize{
#'     \item The \code{module} column value in the target table
#'     \item The CSV filename on OwnCloud (when \code{isCSVBased = TRUE})
#'     \item The filter key for deleting existing entries
#'   }
#' @param isCSVBased Logical indicating the import method:
#'   \describe{
#'     \item{TRUE}{Downloads CSV from OwnCloud WebDAV and pipes through
#'       \code{awk} into \code{psql COPY} for bulk loading}
#'     \item{FALSE}{Uses SQL-based insertion via \code{exportSQL} parameter}
#'   }
#' @param exportSQL Character string containing a SQL SELECT query that
#'   returns the source selection data. Required when \code{isCSVBased = FALSE}.
#'   The query must return columns: \code{sourceid}, \code{eligibilityFlagPhot},
#'   \code{eligibilityFlagRv}. Default is \code{NULL}.
#'
#' @return Called for its side effects (database writes). Returns invisibly.
#'   Messages are emitted reporting deletion counts and import status.
#'
#' @details
#' \strong{CSV-Based Import Pipeline:}
#' When \code{isCSVBased = TRUE}, the function constructs a shell pipeline:
#' \enumerate{
#'   \item \code{curl} downloads the CSV from
#'     \code{gaiaowncloud.isdc.unige.ch/remote.php/webdav/DRC4/FinalValidation/FinalRun/ExportSets/{sosSubName}.csv}
#'     using \code{~/.netrc} credentials
#'   \item \code{awk} transforms the CSV by:
#'     \itemize{
#'       \item Skipping the header row (if it contains "sourceid")
#'       \item Injecting the module name as the second column
#'       \item Passing through existing boolean values for eligibility flags,
#'             or defaulting to \code{false} if the column is empty/missing
#'     }
#'   \item \code{psql COPY FROM STDIN} bulk-loads the transformed data into
#'     the target table on \code{gaiadbgpu03i:55435}
#' }
#'
#' \strong{SQL-Based Import:}
#' When \code{isCSVBased = FALSE}, the function wraps the provided
#' \code{exportSQL} query in an \code{INSERT INTO ... SELECT} statement.
#' NULL eligibility flags are coalesced to \code{FALSE}. The target table
#' is analyzed after insertion for query planner statistics.
#'
#' \strong{Idempotent Operation:}
#' Existing rows for the given module are always deleted before import,
#' making the function safe to re-run without creating duplicates.
#'
#' \strong{Prerequisites:}
#' \itemize{
#'   \item For CSV mode: \code{curl}, \code{awk}, and \code{psql} must be
#'     available on the system PATH, and \code{~/.netrc} must contain
#'     valid OwnCloud credentials
#'   \item For SQL mode: the connection must have permissions to execute
#'     the provided query and insert into the target table
#' }
#'
#' @examples
#' \dontrun{
#' # CSV-based import from OwnCloud
#' import_final_selection(
#'   con = con,
#'   sosSubName = "RR_LYRAE",
#'   isCSVBased = TRUE
#' )
#'
#' # SQL-based import from a subquery
#' import_final_selection(
#'   con = con,
#'   sosSubName = "CEPHEID",
#'   isCSVBased = FALSE,
#'   exportSQL = "SELECT sourceid, eligibilityFlagPhot, eligibilityFlagRv
#'                FROM dr4_ops_cs48_mv.cepheid_candidates
#'                WHERE final_score > 0.8"
#' )
#' }
#'
#' @importFrom DBI dbExecute
#'
#' @export
import_final_selection <- function(con, sosSubName, isCSVBased, exportSQL = NULL) {

  # Delete existing entries first to ensure idempotent imports
  # This prevents duplicate rows when re-running for the same module
  delete_sql <- sprintf(
    "DELETE FROM dr4_ops_cs48_mv.dr4_final_run_selection WHERE module = '%s'",
    sosSubName
  )
  deleted <- dbExecute(con, delete_sql)
  message(sprintf("Deleted %d existing rows for module %s", deleted, sosSubName))

  if (isCSVBased) {
    # CSV import path: curl -> awk -> psql COPY pipeline
    # Build command as single line to avoid shell parsing issues
    cmd <- paste0(
      # Download CSV from OwnCloud WebDAV using netrc credentials
      # --noproxy bypasses proxy for internal host, --insecure skips cert validation
      "curl --noproxy gaiaowncloud.isdc.unige.ch --insecure -X GET --netrc-file ~/.netrc ",
      "'https://gaiaowncloud.isdc.unige.ch/remote.php/webdav/DRC4/FinalValidation/FinalRun/ExportSets/",
      sosSubName, ".csv' | ",
      # Transform CSV: skip header, inject module name, pass through or default eligibility flags
      # - If field $3/$4 contains a value (true/false/t/f/1/0), pass it through
      # - If field is empty or missing, default to "false"
      "awk -F',' -v OFS=',' -v module='", sosSubName, "' '",
      "NR==1 && tolower($0) ~ /sourceid/ { next } ",
      "NR>0 { ",
      "  phot = ($3 != \"\" ? $3 : \"false\"); ",
      "  rv = ($4 != \"\" ? $4 : \"false\"); ",
      "  print $1, module, phot, rv ",
      "}' | ",
      # Bulk-load via psql COPY directly from stdin
      "psql -h gaiadbgpu03i -U dr4_ops_cs48 -p 55435 -d surveys ",
      "-c \"COPY dr4_ops_cs48_mv.dr4_final_run_selection(sourceid,module,eligibilityFlagPhot,eligibilityFlagRv) FROM STDIN WITH CSV\""
    )

    # Log truncated command for debugging (full command may contain sensitive paths)
    message("Executing: ", substr(cmd, 1, 200), "...")
    result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
    message(sprintf("CSV import completed for %s", sosSubName))
    if (length(result) > 0) message(paste(result, collapse = "\n"))

  } else if (!is.null(exportSQL) && nchar(exportSQL) > 0) {
    # SQL import path: INSERT INTO ... SELECT from user-provided subquery
    # Wraps the exportSQL in an INSERT with module name injection
    # and COALESCE to default NULL eligibility flags to FALSE
    sql <- sprintf("
      INSERT INTO dr4_ops_cs48_mv.dr4_final_run_selection (sourceid, module, eligibilityFlagPhot, eligibilityFlagRv)
      SELECT
        sourceid,
        '%s'::text,
        COALESCE(eligibilityFlagPhot, false),
        COALESCE(eligibilityFlagRv, false)
      FROM (%s) AS subq
    ", sosSubName, exportSQL)

    rows <- dbExecute(con, sql)
    # Run ANALYZE to update table statistics after bulk insert
    # Ensures the query planner has accurate cardinality estimates
    rows <- dbExecute(con, "analyze dr4_ops_cs48_mv.dr4_final_run_selection")
    message(sprintf("SQL import completed for %s: %d rows", sosSubName, rows))

  } else {
    # Neither import method was properly configured
    warning("No valid import method: isCSVBased=FALSE and exportSQL not defined")
  }
}
