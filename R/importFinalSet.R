#' Import Final Source Selection into DR4 Operations Table
#'
#' Imports a final run selection for a specific SOS module into the
#' \code{dr4_ops_cs48_mv.dr4_final_run_selection} table. Supports two
#' import methods: CSV-based ingestion from the Gaia OwnCloud WebDAV
#' repository, or direct SQL insertion from a subquery.
#'
#' The function first deletes any existing rows for the specified module
#' to ensure idempotent imports, then inserts new data via the chosen method.
#' Optionally, if a corresponding \code{[sosSubName]_exceptions.csv} file
#' exists on OwnCloud, it is also loaded into the exceptions table.
#'
#' @param con A DBI database connection object to the surveys database.
#'   Must have write access to \code{dr4_ops_cs48_mv.dr4_final_run_selection}
#'   and \code{dr4_ops_cs48_mv.dr4_final_run_selection_exceptions}.
#' @param sosSubName Character string identifying the SOS sub-module name
#'   (e.g., \code{"RR_LYRAE"}, \code{"CEPHEID"}). Used as:
#'   \itemize{
#'     \item The \code{module} column value in the target tables
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
#'   \code{eligibilityFlagRv}, and optionally \code{eligibilityFlagPhotAll}.
#'   Default is \code{NULL}.
#'
#' @return Called for its side effects (database writes). Returns invisibly.
#'   Messages are emitted reporting deletion counts and import status.
#'
#' @details
#' \strong{Main Table Columns:}
#' The target table \code{dr4_final_run_selection} accepts an optional fourth
#' data column \code{eligibilityFlagPhotAll}. In CSV mode, this corresponds to
#' the fourth CSV column, if present. In SQL mode, the \code{exportSQL} query
#' may optionally include it; if absent or NULL it defaults to \code{FALSE}.
#'
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
#'             defaulting to \code{false} if a column is empty or absent
#'       \item Handling the optional \code{eligibilityFlagPhotAll} fourth column
#'             in the same way
#'     }
#'   \item \code{psql COPY FROM STDIN} bulk-loads the transformed data into
#'     the target table on \code{gaiadbgpu03i:55435}
#' }
#'
#' \strong{SQL-Based Import:}
#' When \code{isCSVBased = FALSE}, the function wraps the provided
#' \code{exportSQL} query in an \code{INSERT INTO ... SELECT} statement.
#' NULL eligibility flags are coalesced to \code{FALSE}. The presence of
#' \code{eligibilityFlagPhotAll} in the subquery result is detected at runtime
#' via \code{INFORMATION_SCHEMA} introspection on the subquery columns;
#' if the column is absent it is defaulted to \code{FALSE}.
#' The target table is analyzed after insertion for query planner statistics.
#'
#' \strong{Exceptions File (Optional):}
#' After the main import, the function checks whether a file named
#' \code{[sosSubName]_exceptions.csv} exists in the same OwnCloud folder.
#' If found (HTTP 200), it is loaded into
#' \code{dr4_ops_cs48_mv.dr4_final_run_selection_exceptions} with columns
#' \code{(sourceid, module, source_sos_name, destination_sos_name, optional_comment)}.
#' Existing rows for the module are deleted before loading, mirroring the
#' idempotent behaviour of the main import.
#' The exceptions file is checked and loaded regardless of the \code{isCSVBased}
#' setting for the main table.
#'
#' \strong{Idempotent Operation:}
#' Existing rows for the given module are always deleted before import,
#' making the function safe to re-run without creating duplicates.
#'
#' \strong{Prerequisites:}
#' \itemize{
#'   \item \code{curl}, \code{awk}, and \code{psql} must be available on the
#'     system PATH for both CSV import and exceptions loading
#'   \item \code{~/.netrc} must contain valid OwnCloud credentials
#'   \item For SQL mode: the connection must have permissions to execute
#'     the provided query and insert into the target table
#' }
#'
#' @examples
#' \dontrun{
#' # CSV-based import from OwnCloud (with optional eligibilityFlagPhotAll
#' # column and optional exceptions file auto-detected)
#' import_final_selection(
#'   con = con,
#'   sosSubName = "RR_LYRAE",
#'   isCSVBased = TRUE
#' )
#'
#' # SQL-based import from a subquery including optional eligibilityFlagPhotAll
#' import_final_selection(
#'   con = con,
#'   sosSubName = "CEPHEID",
#'   isCSVBased = FALSE,
#'   exportSQL = "SELECT sourceid, eligibilityFlagPhot, eligibilityFlagRv,
#'                       eligibilityFlagPhotAll
#'                FROM dr4_ops_cs48_mv.cepheid_candidates
#'                WHERE final_score > 0.8"
#' )
#' }
#'
#' @importFrom DBI dbExecute dbGetQuery
#'
#' @export
import_final_selection <- function(con, sosSubName, isCSVBased, exportSQL = NULL) {


 #Shared OwnCloud base settings

  owncloud_base <- paste0(
    "https://gaiaowncloud.isdc.unige.ch/remote.php/webdav/",
    "DRC4/FinalValidation/FinalRun/ExportSets"
  )
  curl_opts <- "--noproxy gaiaowncloud.isdc.unige.ch --insecure --netrc-file ~/.netrc"
  psql_conn <- "-h gaiadbgpu03i -U dr4_ops_cs48 -p 55435 -d surveys"


  # Delete existing entries first to ensure idempotent imports

  delete_sql <- sprintf(
    "DELETE FROM dr4_ops_cs48_mv.dr4_final_run_selection WHERE module = '%s'",
    sosSubName
  )
  deleted <- dbExecute(con, delete_sql)
  message(sprintf("Deleted %d existing rows for module %s", deleted, sosSubName))


  # Main import

  if (isCSVBased) {
    # CSV import path: curl -> awk -> psql COPY pipeline
    #
    # Expected CSV format (with or without quotes, 3 or 4 data columns):
    #   sourceid,eligibilityFlagPhot,eligibilityFlagRv[,eligibilityFlagPhotAll]
    #   1234567890123456789,true,false[,true]
    #
    # awk logic:
    #   - Strip CR characters and surrounding double-quotes from all fields
    #   - Default any missing/empty boolean flag to "false"
    #   - Inject module name as column 2
    #   - Always emit 5 fields so COPY target list matches
    #
    cmd <- paste0(
      "curl ", curl_opts, " -X GET ",
      "'", owncloud_base, "/", sosSubName, ".csv' | ",
      "awk -F, -v OFS=, -v module=\"", sosSubName, "\" '",
      "NR==1 && tolower($0) ~ /sourceid/ { next } ",
      "{ ",
      "  gsub(/\\r/, \"\"); ",
      "  for (i=1; i<=NF; i++) gsub(/\042/, \"\", $i); ",
      "  phot     = ($2 != \"\" ? $2 : \"false\"); ",
      "  rv       = ($3 != \"\" ? $3 : \"false\"); ",
      "  photall  = ($4 != \"\" ? $4 : \"false\"); ",
      "  print $1, module, phot, rv, photall ",
      "}' | ",
      "psql ", psql_conn, " ",
      "-c \"COPY dr4_ops_cs48_mv.dr4_final_run_selection(",
      "sourceid, module, eligibilityFlagPhot, eligibilityFlagRv, eligibilityFlagPhotAll",
      ") FROM STDIN WITH CSV\""
    )

    message("Executing main CSV import for ", sosSubName, " ...")
    message("  CMD: ", substr(cmd, 1, 2000))
    result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
    if (length(result) > 0) message(paste(result, collapse = "\n"))

    dbExecute(con, "VACUUM (FREEZE, ANALYZE) dr4_ops_cs48_mv.dr4_final_run_selection")
    message(sprintf("CSV import completed for %s", sosSubName))

  } else if (!is.null(exportSQL) && nchar(trimws(exportSQL)) > 0) {
    # SQL import path: INSERT INTO ... SELECT from user-provided subquery.
    #
    # Detect whether exportSQL returns eligibilityFlagPhotAll by running the
    # query with LIMIT 0 and inspecting the result column names.
    probe    <- dbGetQuery(con, sprintf("SELECT * FROM (%s) AS _probe LIMIT 0", exportSQL))
    has_photall <- "eligibilityflagphotall" %in% tolower(names(probe))

    photall_expr <- if (has_photall) "COALESCE(eligibilityFlagPhotAll, false)" else "false"

    sql <- sprintf("
      INSERT INTO dr4_ops_cs48_mv.dr4_final_run_selection
        (sourceid, module, eligibilityFlagPhot, eligibilityFlagRv, eligibilityFlagPhotAll)
      SELECT
        sourceid,
        '%s',
        COALESCE(eligibilityFlagPhot,    false),
        COALESCE(eligibilityFlagRv,      false),
        %s
      FROM (%s) AS subq
    ", sosSubName, photall_expr, exportSQL)

    dbExecute(con, "SELECT setdpcganalyticsbase()")
    rows <- dbExecute(con, sql)
    message(sprintf("SQL import completed for %s: %d rows inserted", sosSubName, rows))
    message(sprintf("  eligibilityFlagPhotAll %s", if (has_photall) "taken from subquery" else "defaulted to FALSE"))

    dbExecute(con, "VACUUM (FREEZE, ANALYZE) dr4_ops_cs48_mv.dr4_final_run_selection")

  } else {
    warning("No valid import method: isCSVBased=FALSE and exportSQL not defined")
    return(invisible(NULL))
  }


  # Optional exceptions file: [sosSubName]_exceptions.csv
  #
  # Expected CSV format (with or without header, 5 columns):
  #   sourceid, source_sos_name, destination_sos_name, optional_comment
  # The `module` column is injected by awk (= sosSubName).

  exc_url  <- paste0(owncloud_base, "/", sosSubName, "_exceptions.csv")

  # Use curl --head to check existence without downloading the full file.
  # Exit code 0 + HTTP 2xx means the file is present.
  head_cmd    <- sprintf("curl %s --head --silent --output /dev/null --write-out '%%{http_code}' '%s'",
                         curl_opts, exc_url)
  http_status <- system(head_cmd, intern = TRUE)
  file_exists <- length(http_status) > 0 && grepl("^2", trimws(http_status[length(http_status)]))

  if (file_exists) {
    message(sprintf("Exceptions file found for %s — loading ...", sosSubName))

    # Delete existing exception rows for this module first
    exc_delete_sql <- sprintf(
      "DELETE FROM dr4_ops_cs48_mv.dr4_final_run_selection_exceptions WHERE module = '%s'",
      sosSubName
    )
    exc_deleted <- dbExecute(con, exc_delete_sql)
    message(sprintf("  Deleted %d existing exception rows for module %s", exc_deleted, sosSubName))

    # curl -> awk -> psql COPY pipeline for exceptions
    #
    # CSV column order (after optional header):
    #   sourceid, source_sos_name, destination_sos_name[, optional_comment]
    # awk injects module as column 2, optional_comment defaults to empty string.
    #
    exc_cmd <- paste0(
      "curl ", curl_opts, " -X GET '", exc_url, "' | ",
      "awk -F, -v OFS=, -v module=\"", sosSubName, "\" '",
      "NR==1 && tolower($0) ~ /sourceid/ { next } ",
      "{ ",
      "  gsub(/\\r/, \"\"); ",
      "  for (i=1; i<=NF; i++) gsub(/\042/, \"\", $i); ",
      "  comment = ($4 != \"\" ? $4 : \"\"); ",
      "  print $1, module, $2, $3, comment ",
      "}' | ",
      "psql ", psql_conn, " ",
      "-c \"COPY dr4_ops_cs48_mv.dr4_final_run_selection_exceptions(",
      "sourceid, module, source_module, dest_module, comments",
      ") FROM STDIN WITH CSV\""
    )

    message("  CMD: ", substr(exc_cmd, 1, 2000))
    exc_result <- system(exc_cmd, intern = TRUE, ignore.stderr = FALSE)
    if (length(exc_result) > 0) message(paste(exc_result, collapse = "\n"))

    dbExecute(con, "VACUUM (ANALYZE) dr4_ops_cs48_mv.dr4_final_run_selection_exceptions")
    message(sprintf("Exceptions import completed for %s", sosSubName))

  } else {
    message(sprintf("No exceptions file found for %s (HTTP %s) — skipping.",
                    sosSubName,
                    if (length(http_status) > 0) trimws(http_status[length(http_status)]) else "N/A"))
  }

  invisible(NULL)
}
