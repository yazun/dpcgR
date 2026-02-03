#' @title Parallel Histogram Analysis for PostgreSQL Tables
#' @description
#' Functions for generating histograms of numeric columns in PostgreSQL tables
#' using parallel execution via partParalXZ4. The analysis is performed in two phases:
#' 1. Compute global min/max statistics for all columns (single pass per table)
#' 2. Execute parallel bucketing queries using fixed global boundaries
#'
#' @name histogram_analysis

library(DBI)
library(dplyr)

#' Sanitize Identifier for PostgreSQL
#'
#' Converts a string to a valid PostgreSQL identifier by removing special
#' characters, replacing dashes with underscores, and truncating to max length.
#'
#' @param name Character string to sanitize
#' @param max_length Maximum length of the identifier (default 63, PostgreSQL limit)
#' @return Sanitized identifier string
#' @examples
#' sanitize_identifier("my-table-name")
#' # Returns: "my_table_name"
#' @keywords internal
sanitize_identifier <- function(name, max_length = 63) {
  sanitized <- gsub("-", "_", name)
  sanitized <- gsub("[^a-zA-Z0-9_]", "_", sanitized)
  sanitized <- gsub("_+", "_", sanitized)
  sanitized <- gsub("^_|_$", "", sanitized)
  if (nchar(sanitized) > max_length) {
    sanitized <- substr(sanitized, 1, max_length)
    sanitized <- gsub("_$", "", sanitized)
  }
  return(sanitized)
}

#' Get Numeric Columns for Histogram Generation
#'
#' Queries the database to find all numeric columns (float/int) in tables
#' belonging to the specified module, excluding system columns like
#' runid, catalogid, sourceid, etc.
#'
#' @param conn DBI database connection
#' @param module Module name to filter tables by (matched against dpcg_orm_module_table_mapping)
#' @return Data frame with columns: table_name, column_name, udt_name
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' columns <- get_histogram_columns(conn, "gaia.cu7.algo.sos.CepheidAndRRLyrae")
#' }
#' @export
get_histogram_columns <- function(conn, module) {
  query <- sprintf("
   WITH t AS (
     SELECT tbl.table_name
     FROM dpcg_orm_module_table_mapping tbl
     WHERE '%s' = ANY(tbl.modules)
   ),
   columns_to_histogram AS (
     SELECT
       c.table_name,
       c.column_name,
       c.udt_name
     FROM information_schema.columns c
     JOIN t USING(table_name)
     WHERE c.column_name !~ 'runid|catalogid|sourceid|fstate|sostype|error|other'
       AND c.udt_name ~ '^float|^int'
       AND c.table_schema = current_schema()
     ORDER BY c.table_name, c.column_name
   )
   SELECT * FROM columns_to_histogram
 ", module)

  dbGetQuery(conn, query)
}

#' Build Single-Pass Global Statistics Query
#'
#' Generates a SQL query that computes min, max, NaN count, and valid count
#' for all numeric columns in a table in a single table scan.
#'
#' @param table_name Name of the table to query
#' @param columns_df Data frame of columns (from get_histogram_columns)
#' @param runid Run ID to filter data
#' @param join_clause Optional SQL JOIN clause to limit selection
#' @param table_alias Alias for the main table when using join_clause (default "t")
#' @return SQL query string, or NULL if no columns found
#' @keywords internal
build_global_stats_query <- function(table_name, columns_df, runid,
                                     join_clause = NULL, table_alias = "t") {

  table_cols <- columns_df[columns_df$table_name == table_name, ]

  if (nrow(table_cols) == 0) return(NULL)

  # Build FROM clause
  if (!is.null(join_clause) && nzchar(join_clause)) {
    from_clause <- sprintf("%s %s %s", table_name, table_alias, join_clause)
    col_prefix <- sprintf("%s.", table_alias)
    runid_ref <- sprintf("%s.runid", table_alias)
  } else {
    from_clause <- table_name
    col_prefix <- ""
    runid_ref <- "runid"
  }

  # Build SELECT expressions for ALL columns in a single pass
  select_parts <- sapply(seq_len(nrow(table_cols)), function(i) {
    col <- table_cols[i, ]
    col_name <- col$column_name
    col_ref <- paste0(col_prefix, col_name)

    if (grepl("^float", col$udt_name)) {
      sprintf("
   min(%s) FILTER (WHERE %s IS NOT NULL AND %s != 'NaN'::float8) AS %s_min,
   max(%s) FILTER (WHERE %s IS NOT NULL AND %s != 'NaN'::float8) AS %s_max,
   count(*) FILTER (WHERE %s = 'NaN'::float8) AS %s_nan,
   count(*) FILTER (WHERE %s IS NOT NULL AND %s != 'NaN'::float8) AS %s_valid",
              col_ref, col_ref, col_ref, col_name,
              col_ref, col_ref, col_ref, col_name,
              col_ref, col_name,
              col_ref, col_ref, col_name)
    } else {
      sprintf("
   min(%s) AS %s_min,
   max(%s) AS %s_max,
   0::bigint AS %s_nan,
   count(*) FILTER (WHERE %s IS NOT NULL) AS %s_valid",
              col_ref, col_name,
              col_ref, col_name,
              col_name,
              col_ref, col_name)
    }
  })

  query <- sprintf("SELECT %s\nFROM %s\nWHERE %s = %d",
                   paste(select_parts, collapse = ","),
                   from_clause, runid_ref, runid)

  return(query)
}

#' Pivot Wide Statistics Result to Long Format
#'
#' Converts the wide-format result from build_global_stats_query (one row with
#' columns like col1_min, col1_max, col2_min, ...) to long format (one row per column).
#'
#' @param stats_wide Wide-format data frame from database query
#' @param table_name Name of the source table
#' @param columns_df Data frame of columns (from get_histogram_columns)
#' @return Data frame with columns: table_name, column_name, global_min, global_max, nan_count, non_nan_count
#' @keywords internal
pivot_stats_to_long <- function(stats_wide, table_name, columns_df) {
  table_cols <- columns_df[columns_df$table_name == table_name, ]

  stats_long <- lapply(seq_len(nrow(table_cols)), function(i) {
    col_name <- table_cols$column_name[i]
    data.frame(
      table_name = table_name,
      column_name = col_name,
      global_min = as.numeric(stats_wide[[paste0(col_name, "_min")]]),
      global_max = as.numeric(stats_wide[[paste0(col_name, "_max")]]),
      nan_count = as.numeric(stats_wide[[paste0(col_name, "_nan")]]),
      non_nan_count = as.numeric(stats_wide[[paste0(col_name, "_valid")]]),
      stringsAsFactors = FALSE
    )
  })

  bind_rows(stats_long)
}

#' Compute Global Statistics for All Tables
#'
#' Executes single-pass statistics queries for all tables in the module,
#' computing global min/max and NaN counts for each numeric column.
#'
#' @param conn DBI database connection
#' @param columns_df Data frame of columns (from get_histogram_columns)
#' @param runid Run ID to filter data
#' @param join_clauses Named list of table-specific JOIN clauses
#' @param default_join_clause Default JOIN clause for tables not in join_clauses
#' @return Data frame with global statistics for all columns
#' @keywords internal
compute_global_stats <- function(conn, columns_df, runid,
                                 join_clauses = NULL, default_join_clause = NULL) {

  if (nrow(columns_df) == 0) {
    stop("No columns found for histogram generation")
  }

  tables <- unique(columns_df$table_name)

  cat(sprintf("Computing global stats for %d tables, %d columns (single pass per table)...\n",
              length(tables), nrow(columns_df)))

  all_stats <- list()

  for (tbl in tables) {
    if (!is.null(join_clauses) && tbl %in% names(join_clauses)) {
      join_clause <- join_clauses[[tbl]]
    } else {
      join_clause <- default_join_clause
    }

    query <- build_global_stats_query(tbl, columns_df, runid, join_clause)

    if (!is.null(query)) {
      n_cols <- sum(columns_df$table_name == tbl)
      cat(sprintf("  %s (%d columns)...\n", tbl, n_cols))
      stats_wide <- dbGetQuery(conn, query)
      stats_long <- pivot_stats_to_long(stats_wide, tbl, columns_df)
      all_stats[[tbl]] <- stats_long
    }
  }

  global_stats <- bind_rows(all_stats)
  cat(sprintf("Computed stats for %d columns\n", nrow(global_stats)))

  return(global_stats)
}

#' Build Single Column Bucketing SELECT Statement
#'
#' Generates a SELECT statement for computing histogram buckets for a single column,
#' using precomputed global min/max boundaries. Used as part of UNION ALL query.
#'
#' @param column_name Name of the column
#' @param udt_name PostgreSQL data type (float8, int4, etc.)
#' @param global_min Precomputed global minimum value
#' @param global_max Precomputed global maximum value
#' @param nan_count Precomputed count of NaN values
#' @param non_nan_count Precomputed count of non-NaN values
#' @param num_buckets Number of histogram buckets
#' @param col_ref Column reference (may include table alias prefix)
#' @param table_name Name of the source table
#' @return SQL SELECT statement string
#' @keywords internal
build_column_bucket_select <- function(column_name, udt_name, global_min, global_max,
                                       nan_count, non_nan_count, num_buckets,
                                       col_ref, table_name) {

  nan_count <- as.integer(nan_count)
  non_nan_count <- as.integer(non_nan_count)

  # Handle edge case: all values are the same (or all NULL/NaN)
  if (is.na(global_min) || is.na(global_max) || global_min >= global_max) {
    safe_min <- ifelse(is.na(global_min), 0, global_min)
    safe_max <- ifelse(is.na(global_max), 0, global_max)

    if (grepl("^float", udt_name)) {
      select_expr <- sprintf("
 SELECT
   '%s'::TEXT AS column_name,
   1 AS bucket,
   count(*)::BIGINT AS freq,
   min(%s)::NUMERIC AS bucket_min,
   max(%s)::NUMERIC AS bucket_max,
   avg(%s)::NUMERIC AS bucket_avg,
   %.17g::NUMERIC AS global_min,
   %.17g::NUMERIC AS global_max,
   %d::BIGINT AS nan_count,
   %d::BIGINT AS non_nan_count
 FROM base
 WHERE %s IS NOT NULL AND %s != 'NaN'::float8",
                             column_name,
                             col_ref, col_ref, col_ref,
                             safe_min, safe_max,
                             nan_count, non_nan_count,
                             col_ref, col_ref)
    } else {
      select_expr <- sprintf("
 SELECT
   '%s'::TEXT AS column_name,
   1 AS bucket,
   count(*)::BIGINT AS freq,
   min(%s)::NUMERIC AS bucket_min,
   max(%s)::NUMERIC AS bucket_max,
   avg(%s)::NUMERIC AS bucket_avg,
   %.17g::NUMERIC AS global_min,
   %.17g::NUMERIC AS global_max,
   0::BIGINT AS nan_count,
   %d::BIGINT AS non_nan_count
 FROM base
 WHERE %s IS NOT NULL",
                             column_name,
                             col_ref, col_ref, col_ref,
                             safe_min, safe_max,
                             non_nan_count,
                             col_ref)
    }
  } else {
    # Normal case: use width_bucket with fixed boundaries
    if (grepl("^float", udt_name)) {
      select_expr <- sprintf("
 SELECT
   '%s'::TEXT AS column_name,
   width_bucket(%s, %.17g::float8, %.17g::float8, %d) AS bucket,
   count(*)::BIGINT AS freq,
   min(%s)::NUMERIC AS bucket_min,
   max(%s)::NUMERIC AS bucket_max,
   avg(%s)::NUMERIC AS bucket_avg,
   %.17g::NUMERIC AS global_min,
   %.17g::NUMERIC AS global_max,
   %d::BIGINT AS nan_count,
   %d::BIGINT AS non_nan_count
 FROM base
 WHERE %s IS NOT NULL AND %s != 'NaN'::float8
 GROUP BY width_bucket(%s, %.17g::float8, %.17g::float8, %d)",
                             column_name,
                             col_ref, global_min, global_max, num_buckets,
                             col_ref, col_ref, col_ref,
                             global_min, global_max,
                             nan_count, non_nan_count,
                             col_ref, col_ref,
                             col_ref, global_min, global_max, num_buckets)
    } else {
      select_expr <- sprintf("
 SELECT
   '%s'::TEXT AS column_name,
   width_bucket(%s, %.17g::float8, %.17g::float8, %d) AS bucket,
   count(*)::BIGINT AS freq,
   min(%s)::NUMERIC AS bucket_min,
   max(%s)::NUMERIC AS bucket_max,
   avg(%s)::NUMERIC AS bucket_avg,
   %.17g::NUMERIC AS global_min,
   %.17g::NUMERIC AS global_max,
   0::BIGINT AS nan_count,
   %d::BIGINT AS non_nan_count
 FROM base
 WHERE %s IS NOT NULL
 GROUP BY width_bucket(%s, %.17g::float8, %.17g::float8, %d)",
                             column_name,
                             col_ref, global_min, global_max, num_buckets,
                             col_ref, col_ref, col_ref,
                             global_min, global_max,
                             non_nan_count,
                             col_ref,
                             col_ref, global_min, global_max, num_buckets)
    }
  }

  return(select_expr)
}

#' Build Table Histogram Query
#'
#' Generates a complete SQL query for computing histograms for all columns
#' in a table using a CTE and UNION ALL pattern. Uses precomputed global
#' min/max boundaries to ensure consistent bucketing across parallel chunks.
#'
#' @param table_name Name of the table to query
#' @param columns_df Data frame of columns (from get_histogram_columns)
#' @param global_stats Data frame of global statistics (from compute_global_stats)
#' @param runid Run ID to filter data
#' @param num_buckets Number of histogram buckets (default 20)
#' @param join_clause Optional SQL JOIN clause to limit selection
#' @param table_alias Alias for the main table when using join_clause (default "t")
#' @return SQL query string, or NULL if no columns found
#' @keywords internal
build_table_histogram_query <- function(table_name, columns_df, global_stats, runid,
                                        num_buckets = 20, join_clause = NULL,
                                        table_alias = "t") {

  table_cols <- columns_df[columns_df$table_name == table_name, ]
  table_stats <- global_stats[global_stats$table_name == table_name, ]

  if (nrow(table_cols) == 0) return(NULL)

  # Build FROM clause for CTE
  if (!is.null(join_clause) && nzchar(join_clause)) {
    from_clause <- sprintf("%s %s %s", table_name, table_alias, join_clause)
    col_prefix <- sprintf("%s.", table_alias)
    runid_ref <- sprintf("%s.runid", table_alias)
    sourceid_ref <- sprintf("%s.sourceid", table_alias)
  } else {
    from_clause <- table_name
    col_prefix <- ""
    runid_ref <- "runid"
    sourceid_ref <- "sourceid"
  }

  # Build column list for CTE
  col_list <- paste(sapply(table_cols$column_name, function(cn) {
    paste0(col_prefix, cn)
  }), collapse = ", ")

  # Build CTE
  cte <- sprintf("WITH base AS (
 SELECT %s
 FROM %s
 WHERE %s = %d AND %s = %s
)",
                 col_list, from_clause, runid_ref, runid, sourceid_ref, sourceid_ref)

  # Build UNION ALL of bucket queries for each column
  union_parts <- sapply(seq_len(nrow(table_cols)), function(i) {
    col <- table_cols[i, ]
    stat <- table_stats[table_stats$column_name == col$column_name, ]

    build_column_bucket_select(
      column_name = col$column_name,
      udt_name = col$udt_name,
      global_min = stat$global_min,
      global_max = stat$global_max,
      nan_count = stat$nan_count,
      non_nan_count = stat$non_nan_count,
      num_buckets = num_buckets,
      col_ref = col$column_name,
      table_name = table_name
    )
  })

  # Combine into final query
  query <- sprintf("%s
SELECT
 '%s'::TEXT AS table_name,
 column_name,
 bucket,
 freq,
 bucket_min,
 bucket_max,
 bucket_avg,
 global_min,
 global_max,
 nan_count,
 non_nan_count
FROM (
%s
) all_columns",
                   cte,
                   table_name,
                   paste(union_parts, collapse = "\n UNION ALL\n"))

  return(query)
}

#' Build Aggregation Query for Partial Results
#'
#' Generates a SQL query to aggregate histogram results from parallel execution,
#' summing frequencies and computing weighted averages across chunks.
#'
#' @param partial_table_name Name of the table containing partial results
#' @return SQL query string
#' @keywords internal
build_histogram_aggregation_query <- function(partial_table_name) {
  sprintf("
SELECT
 table_name,
 column_name,
 bucket,
 SUM(freq)::NUMERIC AS freq,
 MIN(bucket_min)::NUMERIC AS bucket_min,
 MAX(bucket_max)::NUMERIC AS bucket_max,
 SUM(bucket_avg * freq) / NULLIF(SUM(freq), 0) AS bucket_avg,
 MIN(global_min)::NUMERIC AS global_min,
 MAX(global_max)::NUMERIC AS global_max,
 MAX(nan_count)::NUMERIC AS nan_count,
 MAX(non_nan_count)::NUMERIC AS non_nan_count
FROM %s
GROUP BY table_name, column_name, bucket
ORDER BY table_name, column_name, bucket",
          partial_table_name)
}

#' Execute Parallel Script via Piped Query
#'
#' Writes the SQL query to a temp file and pipes it to partParalXZ4 for
#' parallel execution across sourceid ranges.
#'
#' @param runid Run ID for partitioning
#' @param output_table Name of the output table to create
#' @param sql_query SQL query to execute
#' @param db_user Database user for execution
#' @param slack_user Slack user for notifications (default "@nienarto")
#' @param parallelism Number of parallel workers (default 80)
#' @param num_chunks Number of data chunks (default 600)
#' @param description Description for logging
#' @return Exit code from shell command (0 = success)
#' @keywords internal
execute_parallel_script <- function(runid, output_table, sql_query, db_user,
                                    slack_user = "@nienarto", parallelism = 80,
                                    num_chunks = 600, description = "Histogram") {

  # Write query to temp file
  query_file <- tempfile(pattern = "query_", fileext = ".sql")
  writeLines(sprintf("---\n\n%s\n\n---", sql_query), query_file)

  # Build command that pipes query file to partParalXZ4
  cmd <- sprintf("cat '%s' | partParalXZ4 %d %s %s %s %d false private cu7_classification '%s' %d",
                 query_file, runid, output_table, db_user, slack_user,
                 parallelism, description, num_chunks)

  exit_code <- system(cmd, intern = FALSE)

  # Clean up
  unlink(query_file)

  return(exit_code)
}

#' Build Histogram Scripts for All Tables
#'
#' Prepares histogram queries and metadata for all tables in the module.
#'
#' @param columns_df Data frame of columns (from get_histogram_columns)
#' @param global_stats Data frame of global statistics (from compute_global_stats)
#' @param runid Run ID to filter data
#' @param schema Output schema for histogram tables (default "dr4_ops_cs48_mv")
#' @param num_buckets Number of histogram buckets (default 20)
#' @param join_clauses Named list of table-specific JOIN clauses
#' @param default_join_clause Default JOIN clause for tables not in join_clauses
#' @return Named list of script info, one entry per table
#' @keywords internal
build_histogram_scripts <- function(columns_df, global_stats, runid,
                                    schema = "dr4_ops_cs48_mv",
                                    num_buckets = 20,
                                    join_clauses = NULL,
                                    default_join_clause = NULL) {

  tables <- unique(columns_df$table_name)

  cat(sprintf("Building histogram queries for %d tables...\n", length(tables)))

  scripts <- list()

  for (tbl in tables) {
    if (!is.null(join_clauses) && tbl %in% names(join_clauses)) {
      join_clause <- join_clauses[[tbl]]
    } else {
      join_clause <- default_join_clause
    }

    sql_query <- build_table_histogram_query(
      table_name = tbl,
      columns_df = columns_df,
      global_stats = global_stats,
      runid = runid,
      num_buckets = num_buckets,
      join_clause = join_clause
    )

    if (is.null(sql_query)) next

    output_table <- sprintf("%s.hist_%s_%d", schema, sanitize_identifier(tbl, 50), runid)
    n_cols <- sum(columns_df$table_name == tbl)

    scripts[[tbl]] <- list(
      sql_query = sql_query,
      source_table = tbl,
      output_table = output_table,
      n_columns = n_cols,
      join_clause = join_clause,
      aggregation_query = build_histogram_aggregation_query(output_table)
    )

    cat(sprintf("  %s: %d columns -> %s\n", tbl, n_cols, output_table))
  }

  return(scripts)
}

#' Execute Histogram Scripts
#'
#' Executes prepared histogram scripts via partParalXZ4 and optionally
#' aggregates the partial results.
#'
#' @param scripts Named list of script info (from build_histogram_scripts)
#' @param runid Run ID for partitioning
#' @param db_user Database user for execution
#' @param conn DBI database connection for aggregation (optional)
#' @param slack_user Slack user for notifications (default "@nienarto")
#' @param parallelism Number of parallel workers (default 80)
#' @param num_chunks Number of data chunks (default 600)
#' @param execute If TRUE, execute scripts; if FALSE, return scripts only
#' @param debug If TRUE, print detailed debug output
#' @return List with execution results and combined histograms
#' @keywords internal
execute_histogram_scripts <- function(scripts, runid, db_user, conn = NULL,
                                      slack_user = "@nienarto", parallelism = 80,
                                      num_chunks = 600, execute = FALSE, debug = FALSE) {
  results <- list()
  all_histograms <- list()

  for (i in seq_along(scripts)) {
    script_info <- scripts[[i]]
    tbl <- names(scripts)[i]

    if (debug) {
      cat(sprintf("\n=== TABLE %d/%d: %s ===\n", i, length(scripts), tbl))
      cat(sprintf("Output table: %s\n", script_info$output_table))
      cat(sprintf("Columns: %d\n", script_info$n_columns))
      if (!is.null(script_info$join_clause) && nzchar(script_info$join_clause)) {
        cat(sprintf("Join clause: %s\n", script_info$join_clause))
      }
      cat("\n--- Query (first 2000 chars) ---\n")
      cat(substr(script_info$sql_query, 1, 2000))
      if (nchar(script_info$sql_query) > 2000) cat("\n... [truncated]")
      cat("\n==================\n\n")
    }

    if (execute) {
      cat(sprintf("Executing %d/%d: %s (%d columns)...\n",
                  i, length(scripts), tbl, script_info$n_columns))

      exit_code <- execute_parallel_script(
        runid = runid,
        output_table = script_info$output_table,
        sql_query = script_info$sql_query,
        db_user = db_user,
        slack_user = slack_user,
        parallelism = parallelism,
        num_chunks = num_chunks,
        description = sprintf("Histogram %s", tbl)
      )

      if (exit_code != 0) {
        warning(sprintf("Script for %s failed with exit code %d", tbl, exit_code))
        results[[tbl]] <- list(success = FALSE, exit_code = exit_code)
      } else {
        results[[tbl]] <- list(
          success = TRUE,
          source_table = tbl,
          output_table = script_info$output_table,
          n_columns = script_info$n_columns
        )

        if (!is.null(conn)) {
          cat(sprintf("  Aggregating results for %s...\n", tbl))
          agg_result <- dbGetQuery(conn, script_info$aggregation_query)
          results[[tbl]]$histogram_data <- agg_result
          all_histograms[[tbl]] <- agg_result
        }
      }
    } else {
      results[[tbl]] <- list(
        sql_query = script_info$sql_query,
        source_table = tbl,
        output_table = script_info$output_table,
        n_columns = script_info$n_columns,
        aggregation_query = script_info$aggregation_query
      )
    }
  }

  if (execute && length(all_histograms) > 0) {
    results$combined_histograms <- bind_rows(all_histograms)
  }

  return(results)
}

#' Compute Bucket Boundaries for Visualization
#'
#' Adds computed bucket boundary columns to histogram data for plotting,
#' including bucket_lower, bucket_upper, bucket_center, bucket_width, and freq_pct.
#'
#' @param histogram_df Data frame with histogram data
#' @param num_buckets Number of histogram buckets (must match original query)
#' @return Data frame with additional boundary columns
#' @export
compute_bucket_boundaries <- function(histogram_df, num_buckets = 20) {
  histogram_df %>%
    mutate(
      # Ensure numeric types (in case of integer64 from DB)
      freq = as.numeric(freq),
      nan_count = as.numeric(nan_count),
      non_nan_count = as.numeric(non_nan_count),
      global_min = as.numeric(global_min),
      global_max = as.numeric(global_max)
    ) %>%
    group_by(table_name, column_name) %>%
    mutate(
      bucket_width = (global_max - global_min) / num_buckets,
      bucket_lower = global_min + (bucket - 1) * bucket_width,
      bucket_upper = global_min + bucket * bucket_width,
      bucket_center = (bucket_lower + bucket_upper) / 2,
      freq_pct = freq / sum(freq) * 100
    ) %>%
    ungroup()
}

#' Run Histogram Analysis
#'
#' Main workflow function for generating histograms of numeric columns in
#' PostgreSQL tables using parallel execution. The analysis is performed
#' in two phases:
#'
#' 1. **Phase 1**: Compute global min/max statistics for all columns
#'    (single table scan per table, not parallelized)
#' 2. **Phase 2**: Execute parallel bucketing queries using fixed global
#'    boundaries via partParalXZ4
#'
#' The two-phase approach ensures consistent bucket boundaries across all
#' parallel chunks, allowing correct aggregation of partial results.
#'
#' @param inparams List with database connection parameters:
#'   \itemize{
#'     \item hostname: Database hostname
#'     \item dbPort: Database port
#'     \item dbUser: Database user
#'   }
#' @param runid Run ID to filter data
#' @param module Module name to filter tables (matched against dpcg_orm_module_table_mapping)
#' @param schema Output schema for histogram tables (default "dr4_ops_cs48_mv")
#' @param num_buckets Number of histogram buckets (default 20)
#' @param join_clauses Named list of table-specific JOIN clauses, e.g.,
#'   \code{list(sos_cepheidsattributes = "JOIN selection_table USING (sourceid)")}
#' @param default_join_clause Default JOIN clause applied to tables not in join_clauses
#' @param slack_user Slack user for notifications (default "@nienarto")
#' @param parallelism Number of parallel workers for partParalXZ4 (default 80)
#' @param num_chunks Number of data chunks for partParalXZ4 (default 600)
#' @param execute If TRUE, execute scripts; if FALSE, return scripts only (dry run)
#' @param debug If TRUE, print detailed debug output
#' @return List containing:
#'   \itemize{
#'     \item Per-table results with success status and histogram data
#'     \item combined_histograms: All histograms in one data frame
#'     \item histogram_for_viz: Combined histograms with computed bucket boundaries
#'     \item metadata: Analysis metadata (runid, module, num_buckets, etc.)
#'   }
#'
#' @examples
#' \dontrun{
#' # Dry run - see generated queries without execution
#' results <- run_histogram_analysis(
#'   inparams = params,
#'   runid = 90005,
#'   module = "gaia.cu7.algo.sos.CepheidAndRRLyrae.CepheidAndRrLyrae",
#'   num_buckets = 100,
#'   execute = FALSE,
#'   debug = TRUE
#' )
#'
#' # Execute histogram generation
#' results <- run_histogram_analysis(
#'   inparams = params,
#'   runid = 90005,
#'   module = "gaia.cu7.algo.sos.CepheidAndRRLyrae.CepheidAndRrLyrae",
#'   num_buckets = 100,
#'   execute = TRUE
#' )
#'
#' # With table-specific join clause to limit selection
#' results <- run_histogram_analysis(
#'   inparams = params,
#'   runid = 90005,
#'   module = "gaia.cu7.algo.sos.CepheidAndRRLyrae.CepheidAndRrLyrae",
#'   join_clauses = list(
#'     sos_cepheidsattributes = "JOIN my_selection USING (sourceid)"
#'   ),
#'   execute = TRUE
#' )
#'
#' # Access results
#' results$histogram_for_viz  # Data ready for plotting
#' results$metadata$global_stats  # Global min/max statistics
#' }
#'
#' @export
run_histogram_analysis <- function(inparams, runid, module,
                                   schema = "dr4_ops_cs48_mv",
                                   num_buckets = 20,
                                   join_clauses = NULL,
                                   default_join_clause = NULL,
                                   slack_user = "@nienarto",
                                   parallelism = 80,
                                   num_chunks = 600,
                                   execute = FALSE,
                                   debug = FALSE) {

  conn <- dpcgR::connect(hostname = inparams$hostname, port = inparams$dbPort, user = inparams$dbUser)

  cat(sprintf("=== HISTOGRAM ANALYSIS FOR MODULE '%s', RUNID %d ===\n\n", module, runid))
  cat(sprintf("Using database user: %s\n\n", inparams$dbUser))

  # Get column metadata
  columns_df <- get_histogram_columns(conn, module)
  cat(sprintf("Found %d columns across %d tables\n\n",
              nrow(columns_df), length(unique(columns_df$table_name))))

  # PHASE 1: Compute global statistics (single pass per table)
  cat("=== PHASE 1: Computing global statistics (single pass per table) ===\n")
  global_stats <- compute_global_stats(
    conn = conn,
    columns_df = columns_df,
    runid = runid,
    join_clauses = join_clauses,
    default_join_clause = default_join_clause
  )
  cat("\n")

  # PHASE 2: Build and execute parallel bucketing scripts
  cat("=== PHASE 2: Building histogram queries (one per table) ===\n")
  scripts <- build_histogram_scripts(
    columns_df = columns_df,
    global_stats = global_stats,
    runid = runid,
    schema = schema,
    num_buckets = num_buckets,
    join_clauses = join_clauses,
    default_join_clause = default_join_clause
  )
  cat("\n")

  if (execute) {
    cat("=== PHASE 2: Executing parallel bucketing scripts ===\n")
    results <- execute_histogram_scripts(
      scripts = scripts,
      runid = runid,
      db_user = inparams$dbUser,
      conn = conn,
      slack_user = slack_user,
      parallelism = parallelism,
      num_chunks = num_chunks,
      execute = TRUE,
      debug = debug
    )

    if (!is.null(results$combined_histograms) && nrow(results$combined_histograms) > 0) {
      results$histogram_for_viz <- compute_bucket_boundaries(results$combined_histograms, num_buckets)
    }
  } else {
    results <- execute_histogram_scripts(
      scripts = scripts,
      runid = runid,
      db_user = inparams$dbUser,
      conn = NULL,
      execute = FALSE,
      debug = debug
    )
  }

  dbDisconnect(conn)

  cat("\n=== HISTOGRAM ANALYSIS COMPLETE ===\n")

  results$metadata <- list(
    runid = runid,
    module = module,
    num_buckets = num_buckets,
    n_tables = length(scripts),
    n_columns = nrow(columns_df),
    tables = names(scripts),
    global_stats = global_stats,
    db_user = inparams$dbUser
  )

  return(results)
}
