#' @title Parallel Histogram Analysis for PostgreSQL Tables
#' @description
#' Functions for generating histograms of numeric columns in PostgreSQL tables
#' using parallel execution via partParalXZ4. The analysis is performed in two phases:
#' 1. Compute global min/max statistics for all columns (parallel execution per table)
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

#' Expand Array Columns into Per-Element Rows
#'
#' Transforms a columns data frame by expanding array-type columns into
#' separate rows for the first \code{n} elements plus the array length.
#' Scalar columns pass through unchanged.
#'
#' For each array column, \code{n + 1} virtual rows are created:
#' \itemize{
#'   \item \code{colname_1} through \code{colname_n}: individual elements
#'     (\code{colname[1]} ... \code{colname[n]}), each filtered to arrays
#'     with length >= that index
#'   \item \code{colname_len}: array length (\code{cardinality(colname)}), treated as integer
#' }
#'
#' @param columns_df Data frame with columns: table_name, column_name, udt_name
#' @param n Number of array elements to expand per column (default 10)
#' @return Data frame with columns: table_name, column_name, udt_name, col_ref,
#'   min_array_len, array_col. \code{col_ref} contains the SQL expression
#'   (e.g., \code{colname[1]}). \code{min_array_len} is the minimum array
#'   cardinality required (0 for scalars and _len, 1..n for array elements).
#' @keywords internal
expand_array_columns <- function(columns_df, n = 10) {
  if (nrow(columns_df) == 0) {
    columns_df$col_ref <- character(0)
    columns_df$min_array_len <- integer(0)
    columns_df$array_col <- character(0)
    columns_df$array_len_ref <- character(0)
    return(columns_df)
  }

  expanded <- lapply(seq_len(nrow(columns_df)), function(i) {
    row <- columns_df[i, ]
    if (grepl("^_", row$udt_name)) {
      # Array type: expand into elements [1]..[n] and cardinality
      base_udt <- sub("^_", "", row$udt_name)
      cn <- row$column_name
      # Element rows: colname_1 .. colname_n
      elem_names <- paste0(cn, "_", seq_len(n))
      elem_refs <- sprintf("%s[%d]", cn, seq_len(n))
      elem_udts <- rep(base_udt, n)
      elem_min_len <- seq_len(n)
      # Length row: colname_len
      # array_len_ref stores the raw expression (without cardinality() wrapper)
      # so the CTE builder can apply table prefix correctly
      data.frame(
        table_name = rep(row$table_name, n + 1),
        column_name = c(elem_names, paste0(cn, "_len")),
        udt_name = c(elem_udts, "int4"),
        col_ref = c(elem_refs, sprintf("cardinality(%s)", cn)),
        min_array_len = c(as.integer(elem_min_len), 0L),
        array_col = rep(cn, n + 1),
        array_len_ref = c(rep(cn, n), NA_character_),
        stringsAsFactors = FALSE
      )
    } else {
      # Scalar type: col_ref is just the column_name
      data.frame(
        table_name = row$table_name,
        column_name = row$column_name,
        udt_name = row$udt_name,
        col_ref = row$column_name,
        min_array_len = 0L,
        array_col = NA_character_,
        array_len_ref = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  })
  bind_rows(expanded)
}

#' Introspect Composite Type Fields
#'
#' Queries PostgreSQL system catalogs (pg_attribute, pg_type) to discover
#' the fields of a composite type and their types/categories.
#'
#' @param conn DBI database connection
#' @param type_name Character string: the composite type name (e.g., "tmdb_cu7_modelresult_149")
#' @return Data frame with columns: field_name, field_type, field_category
#'   (N=numeric, A=array, S=string, C=composite, B=boolean)
#' @keywords internal
introspect_composite_type <- function(conn, type_name) {
  # Strip leading underscore for array-of-composite types
  base_type <- sub("^_", "", type_name)

  query <- sprintf("
    SELECT a.attname AS field_name,
           ft.typname AS field_type,
           ft.typcategory AS field_category
    FROM pg_type ct
    JOIN pg_class cl ON cl.oid = ct.typrelid
    JOIN pg_attribute a ON a.attrelid = cl.oid AND a.attnum > 0 AND NOT a.attisdropped
    JOIN pg_type ft ON ft.oid = a.atttypid
    WHERE ct.typname = '%s'
    ORDER BY a.attnum
  ", base_type)

  dbGetQuery(conn, query)
}

#' Expand Composite Type Columns into Per-Field Rows
#'
#' Takes composite-type columns and expands them into individual field rows
#' with appropriate \code{col_ref} SQL expressions for field access.
#' Handles scalar composites, arrays of composites, and arrays within composites.
#'
#' @param conn DBI database connection (for type introspection)
#' @param columns_df Data frame with table_name, column_name, udt_name
#' @param n Number of array elements to expand (default 10)
#' @param max_depth Maximum recursion depth for nested composites (default 2)
#' @return Data frame with columns: table_name, column_name, udt_name, col_ref,
#'   min_array_len, array_col, array_len_ref
#' @keywords internal
expand_composite_columns <- function(conn, columns_df, n = 10, max_depth = 2) {
  if (nrow(columns_df) == 0) {
    return(data.frame(
      table_name = character(0), column_name = character(0),
      udt_name = character(0), col_ref = character(0),
      min_array_len = integer(0), array_col = character(0),
      array_len_ref = character(0), stringsAsFactors = FALSE
    ))
  }

  # Cache type introspection results
  type_cache <- list()
  get_fields <- function(type_name) {
    base_type <- sub("^_", "", type_name)
    if (is.null(type_cache[[base_type]])) {
      type_cache[[base_type]] <<- introspect_composite_type(conn, base_type)
    }
    type_cache[[base_type]]
  }

  # Recursive helper: expand fields of a composite access expression
  # col_expr: SQL expression to access the composite (e.g., "col" or "(col)[1]")
  # name_prefix: display name prefix (e.g., "col__" or "col_1__")
  # type_name: composite type name
  # depth: current recursion depth
  # base_array_col: top-level array column (for min_array_len tracking), or NA
  # base_min_len: min_array_len from parent (for array-of-composite elements)
  # base_len_ref: array_len_ref from parent
  expand_fields <- function(table_name, col_expr, name_prefix, type_name,
                            depth, base_array_col, base_min_len, base_len_ref) {
    fields <- get_fields(type_name)
    if (nrow(fields) == 0) return(list())

    rows <- list()
    for (j in seq_len(nrow(fields))) {
      f <- fields[j, ]
      field_ref <- sprintf("(%s).%s", col_expr, f$field_name)
      field_name <- paste0(name_prefix, f$field_name)

      if (f$field_category == "N") {
        # Numeric field: direct histogram
        rows[[length(rows) + 1]] <- data.frame(
          table_name = table_name,
          column_name = field_name,
          udt_name = f$field_type,
          col_ref = field_ref,
          min_array_len = base_min_len,
          array_col = base_array_col,
          array_len_ref = base_len_ref,
          stringsAsFactors = FALSE
        )
      } else if (f$field_category == "A") {
        # Array field inside composite: expand elements
        base_elem_type <- sub("^_", "", f$field_type)
        arr_name_prefix <- paste0(name_prefix, f$field_name)
        # Raw expression for the array (no cardinality wrapper — CTE builder adds it)
        arr_raw_expr <- sprintf("(%s).%s", col_expr, f$field_name)

        for (k in seq_len(n)) {
          elem_ref <- sprintf("(%s).%s[%d]", col_expr, f$field_name, k)
          elem_name <- sprintf("%s_%d", arr_name_prefix, k)
          rows[[length(rows) + 1]] <- data.frame(
            table_name = table_name,
            column_name = elem_name,
            udt_name = base_elem_type,
            col_ref = elem_ref,
            min_array_len = as.integer(k),
            array_col = arr_name_prefix,
            array_len_ref = arr_raw_expr,
            stringsAsFactors = FALSE
          )
        }
        # Array length row
        rows[[length(rows) + 1]] <- data.frame(
          table_name = table_name,
          column_name = paste0(arr_name_prefix, "_len"),
          udt_name = "int4",
          col_ref = sprintf("cardinality(%s)", arr_raw_expr),
          min_array_len = base_min_len,
          array_col = base_array_col,
          array_len_ref = base_len_ref,
          stringsAsFactors = FALSE
        )
      } else if (f$field_category == "C" && depth < max_depth) {
        # Nested composite: recurse
        nested <- expand_fields(table_name, field_ref,
                                paste0(field_name, "__"), f$field_type,
                                depth + 1, base_array_col, base_min_len, base_len_ref)
        rows <- c(rows, nested)
      }
      # Skip other categories (S=string, B=boolean, etc.) — handled by text pipeline
    }
    rows
  }

  all_rows <- list()
  for (i in seq_len(nrow(columns_df))) {
    row <- columns_df[i, ]
    is_array <- grepl("^_", row$udt_name)
    base_type <- sub("^_", "", row$udt_name)
    col <- row$column_name

    if (is_array) {
      # Array of composites: expand each element, then each field
      for (k in seq_len(n)) {
        elem_expr <- sprintf("(%s)[%d]", col, k)
        elem_prefix <- sprintf("%s_%d__", col, k)
        field_rows <- expand_fields(
          row$table_name, elem_expr, elem_prefix, base_type,
          depth = 1, base_array_col = col,
          base_min_len = as.integer(k),
          base_len_ref = col  # raw expression, CTE builder wraps with cardinality()
        )
        all_rows <- c(all_rows, field_rows)
      }
      # Array length row for the top-level array
      all_rows[[length(all_rows) + 1]] <- data.frame(
        table_name = row$table_name,
        column_name = paste0(col, "_len"),
        udt_name = "int4",
        col_ref = sprintf("cardinality(%s)", col),
        min_array_len = 0L,
        array_col = NA_character_,
        array_len_ref = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      # Scalar composite: expand each field directly
      field_rows <- expand_fields(
        row$table_name, col, paste0(col, "__"), base_type,
        depth = 1, base_array_col = NA_character_,
        base_min_len = 0L, base_len_ref = NA_character_
      )
      all_rows <- c(all_rows, field_rows)
    }
  }

  if (length(all_rows) == 0) {
    return(data.frame(
      table_name = character(0), column_name = character(0),
      udt_name = character(0), col_ref = character(0),
      min_array_len = integer(0), array_col = character(0),
      array_len_ref = character(0), stringsAsFactors = FALSE
    ))
  }

  bind_rows(all_rows)
}

#' Detect System Columns Per Table
#'
#' Queries \code{information_schema.columns} to determine which system columns
#' (runid, catalogid, sourceid) exist for each table. Returns a data frame
#' with one row per table and boolean flags for each system column.
#'
#' @param conn DBI database connection
#' @param table_names Character vector of table names to check
#' @return Data frame with columns: table_name, has_runid, has_catalogid, has_sourceid
#' @keywords internal
detect_system_columns <- function(conn, table_names, schemas = NULL) {
  if (length(table_names) == 0) {
    return(data.frame(
      table_name = character(0),
      has_runid = logical(0),
      has_catalogid = logical(0),
      has_sourceid = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  tables_sql <- paste(sprintf("'%s'", table_names), collapse = ", ")

  # Build schema filter — check current_schema() plus any additional schemas
  if (!is.null(schemas) && length(schemas) > 0) {
    schema_sql <- paste(sprintf("'%s'", schemas), collapse = ", ")
    schema_filter <- sprintf("c.table_schema IN (current_schema(), %s)", schema_sql)
  } else {
    schema_filter <- "c.table_schema IN (current_schema(), current_schema()||'_mdb')"
  }

  query <- sprintf("
    SELECT
      c.table_name,
      bool_or(c.column_name = 'runid') AS has_runid,
      bool_or(c.column_name = 'catalogid') AS has_catalogid,
      bool_or(c.column_name = 'sourceid') AS has_sourceid
    FROM information_schema.columns c
    WHERE c.table_name IN (%s)
      AND %s
      AND c.column_name IN ('runid', 'catalogid', 'sourceid')
    GROUP BY c.table_name
  ", tables_sql, schema_filter)

  result <- dbGetQuery(conn, query)

  # Ensure all tables are represented, even if they have none of these columns
  all_tables <- data.frame(table_name = table_names, stringsAsFactors = FALSE)
  merged <- merge(all_tables, result, by = "table_name", all.x = TRUE)
  merged$has_runid[is.na(merged$has_runid)] <- FALSE
  merged$has_catalogid[is.na(merged$has_catalogid)] <- FALSE
  merged$has_sourceid[is.na(merged$has_sourceid)] <- FALSE

  return(merged)
}

#' Estimate Table Row Counts
#'
#' Estimates row counts for partitioned tables by summing
#' \code{pg_class.reltuples} across all partitions (child tables found
#' via \code{pg_inherits}). For non-partitioned tables, uses the parent's
#' own \code{reltuples} directly.
#'
#' @param conn DBI database connection
#' @param table_names Character vector of table names
#' @return Named numeric vector of estimated row counts, keyed by table_name
#' @keywords internal
estimate_table_rows <- function(conn, table_names) {
  if (length(table_names) == 0) return(setNames(numeric(0), character(0)))

  tables_sql <- paste(sprintf("'%s'", table_names), collapse = ", ")
  query <- sprintf("
    SELECT parent.relname AS table_name,
           COALESCE(sum(child.reltuples), parent.reltuples)::BIGINT AS est_rows
    FROM pg_class parent
    JOIN pg_namespace n ON n.oid = parent.relnamespace
    LEFT JOIN pg_inherits inh ON inh.inhparent = parent.oid
    LEFT JOIN pg_class child ON child.oid = inh.inhrelid
    WHERE parent.relname IN (%s)
      AND n.nspname IN (current_schema(), current_schema()||'_mdb')
    GROUP BY parent.relname, parent.reltuples
  ", tables_sql)

  result <- tryCatch(
    dbGetQuery(conn, query),
    error = function(e) {
      warning(sprintf("Could not estimate table rows: %s", e$message))
      data.frame(table_name = character(0), est_rows = numeric(0))
    }
  )

  # De-duplicate: if same table_name in multiple schemas, take the max
  if (nrow(result) > 0) {
    result <- result %>%
      group_by(table_name) %>%
      summarise(est_rows = max(est_rows), .groups = "drop")
  }

  row_counts <- setNames(as.numeric(result$est_rows), result$table_name)

  # Ensure all requested tables are in the result (default 0 if not found)
  missing <- setdiff(table_names, names(row_counts))
  if (length(missing) > 0) {
    row_counts[missing] <- 0
  }

  return(row_counts)
}

#' Get Numeric Columns for Histogram Generation
#'
#' Queries the database to find all numeric columns (float/int) in tables
#' belonging to the specified module, excluding system columns like
#' runid, catalogid, sourceid, etc.
#'
#' @param conn DBI database connection
#' @param module Module name to filter tables by (matched against dpcg_orm_module_table_mapping)
#' @param array_elements Number of array elements to expand per array column (default 10)
#' @return List with two elements:
#'   \itemize{
#'     \item \code{columns}: Data frame with columns: table_name, column_name, udt_name, col_ref.
#'       For array columns, each column is expanded into \code{array_elements} rows (one per element)
#'       plus a length row.
#'     \item \code{table_info}: Data frame with columns: table_name, has_runid, has_catalogid, has_sourceid.
#'   }
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' result <- get_histogram_columns(conn, "gaia.cu7.algo.sos.CepheidAndRRLyrae")
#' result$columns   # column metadata
#' result$table_info # system column presence per table
#' }
#' @export
get_histogram_columns <- function(conn, module, array_elements = 10) {
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
       AND c.udt_name ~ '^float|^int|^_float|^_int'
       AND c.table_schema = current_schema()
     ORDER BY c.table_name, c.column_name
   )
   SELECT * FROM columns_to_histogram
 ", module)

  raw_df <- dbGetQuery(conn, query)
  columns_df <- expand_array_columns(raw_df, n = array_elements)

  # Discover composite-type columns and expand their numeric fields
  composite_query <- sprintf("
   WITH t AS (
     SELECT tbl.table_name
     FROM dpcg_orm_module_table_mapping tbl
     WHERE '%s' = ANY(tbl.modules)
   )
   SELECT c.table_name, c.column_name, c.udt_name
   FROM information_schema.columns c
   JOIN t USING(table_name)
   WHERE c.column_name !~ 'runid|catalogid|sourceid|fstate|sostype|error|other'
     AND c.udt_name IN (
       SELECT typname FROM pg_type WHERE typtype = 'c'
       UNION ALL
       SELECT '_' || typname FROM pg_type WHERE typtype = 'c'
     )
     AND c.table_schema = current_schema()
   ORDER BY c.table_name, c.column_name
  ", module)

  composite_raw <- dbGetQuery(conn, composite_query)
  if (nrow(composite_raw) > 0) {
    composite_expanded <- expand_composite_columns(conn, composite_raw, n = array_elements)
    # Keep only numeric-type expanded fields
    composite_numeric <- composite_expanded[
      grepl("^float|^int|^numeric", composite_expanded$udt_name), , drop = FALSE
    ]
    if (nrow(composite_numeric) > 0) {
      # Ensure columns_df also has array_len_ref for bind_rows compatibility
      if (!"array_len_ref" %in% names(columns_df)) {
        columns_df$array_len_ref <- NA_character_
      }
      columns_df <- bind_rows(columns_df, composite_numeric)
      message(sprintf("  Discovered %d numeric fields from %d composite columns",
                      nrow(composite_numeric), nrow(composite_raw)))
    }
  }

  all_tables <- unique(c(raw_df$table_name,
                          if (nrow(composite_raw) > 0) composite_raw$table_name))
  table_info <- detect_system_columns(conn, all_tables)

  list(columns = columns_df, table_info = table_info)
}



#' Get Numeric Columns for Histogram Generation
#'
#' Queries the database to find all numeric columns (float/int) in tables
#' belonging to the specified module, excluding system columns like
#' runid, catalogid, sourceid, etc. for any given table (module)
#'
#' @param conn DBI database connection
#' @param module Module name to filter tables by - effectively a DB table
#' @param array_elements Number of array elements to expand per array column (default 10)
#' @return List with two elements:
#'   \itemize{
#'     \item \code{columns}: Data frame with columns: table_name, column_name, udt_name, col_ref.
#'       For array columns, each column is expanded into \code{array_elements} rows (one per element)
#'       plus a length row.
#'     \item \code{table_info}: Data frame with columns: table_name, has_runid, has_catalogid, has_sourceid.
#'   }
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(...)
#' result <- get_histogram_mdb_columns(conn, "gaia.cu7.algo.sos.CepheidAndRRLyrae")
#' result$columns   # column metadata
#' result$table_info # system column presence per table
#' }
#' @export
get_histogram_mdb_columns <- function(conn, module, array_elements = 10) {
  query <- sprintf("
   WITH t AS (
     SELECT '%s' table_name
   ),
   columns_to_histogram AS (
     SELECT
       c.table_name,
       c.column_name,
       c.udt_name
     FROM information_schema.columns c
     JOIN t USING(table_name)
     WHERE c.column_name !~ 'runid|catalogid|sourceid|fstate|sostype|error|other|file_id|transfer_id'
       AND c.udt_name ~ '^float|^int|^_float|^_int'
       AND c.table_schema in (current_schema(),current_schema()||'_mdb')
     ORDER BY c.table_name, c.column_name
   )
   SELECT * FROM columns_to_histogram
 ", module)

  raw_df <- dbGetQuery(conn, query)
  columns_df <- expand_array_columns(raw_df, n = array_elements)

  # Discover composite-type columns and expand their numeric fields
  composite_query <- sprintf("
   WITH t AS (
     SELECT '%s' table_name
   )
   SELECT c.table_name, c.column_name, c.udt_name
   FROM information_schema.columns c
   JOIN t USING(table_name)
   WHERE c.column_name !~ 'runid|catalogid|sourceid|fstate|sostype|error|other|file_id|transfer_id'
     AND c.udt_name IN (
       SELECT typname FROM pg_type WHERE typtype = 'c'
       UNION ALL
       SELECT '_' || typname FROM pg_type WHERE typtype = 'c'
     )
     AND c.table_schema in (current_schema(),current_schema()||'_mdb')
   ORDER BY c.table_name, c.column_name
  ", module)

  composite_raw <- dbGetQuery(conn, composite_query)
  if (nrow(composite_raw) > 0) {
    composite_expanded <- expand_composite_columns(conn, composite_raw, n = array_elements)
    composite_numeric <- composite_expanded[
      grepl("^float|^int|^numeric", composite_expanded$udt_name), , drop = FALSE
    ]
    if (nrow(composite_numeric) > 0) {
      if (!"array_len_ref" %in% names(columns_df)) {
        columns_df$array_len_ref <- NA_character_
      }
      columns_df <- bind_rows(columns_df, composite_numeric)
      message(sprintf("  Discovered %d numeric fields from %d composite columns",
                      nrow(composite_numeric), nrow(composite_raw)))
    }
  }

  all_tables <- unique(c(raw_df$table_name,
                          if (nrow(composite_raw) > 0) composite_raw$table_name))
  table_info <- detect_system_columns(conn, all_tables)

  list(columns = columns_df, table_info = table_info)
}


#' Get Text Columns for Categorical Histogram Generation
#'
#' Queries the database to find all text/categorical and boolean columns in
#' tables belonging to the specified module, excluding system columns.
#'
#' @param conn DBI database connection
#' @param module Module name to filter tables by (matched against dpcg_orm_module_table_mapping)
#' @return List with two elements:
#'   \itemize{
#'     \item \code{columns}: Data frame with columns: table_name, column_name, udt_name, col_ref,
#'       min_array_len, array_col.
#'     \item \code{table_info}: Data frame with columns: table_name, has_runid, has_catalogid, has_sourceid.
#'   }
#' @export
get_histogram_text_columns <- function(conn, module) {
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
     WHERE c.column_name !~ 'runid|catalogid|sourceid|fstate|sostype|error|other|file_id|transfer_id'
       AND c.udt_name IN ('text', 'varchar', 'bpchar', 'name', 'bool')
       AND c.table_schema = current_schema()
     ORDER BY c.table_name, c.column_name
   )
   SELECT * FROM columns_to_histogram
 ", module)

  raw_df <- dbGetQuery(conn, query)
  if (nrow(raw_df) > 0) {
    raw_df$col_ref <- raw_df$column_name
    raw_df$min_array_len <- 0L
    raw_df$array_col <- NA_character_
  } else {
    raw_df$col_ref <- character(0)
    raw_df$min_array_len <- integer(0)
    raw_df$array_col <- NA_character_
  }
  table_info <- detect_system_columns(conn, unique(raw_df$table_name))
  list(columns = raw_df, table_info = table_info)
}


#' Get Text Columns for Categorical Histogram Generation (Single Table)
#'
#' Queries the database to find all text/categorical and boolean columns in
#' a single table, excluding system columns.
#'
#' @param conn DBI database connection
#' @param module Module name (table name) to query
#' @return List with two elements:
#'   \itemize{
#'     \item \code{columns}: Data frame with columns: table_name, column_name, udt_name, col_ref,
#'       min_array_len, array_col.
#'     \item \code{table_info}: Data frame with columns: table_name, has_runid, has_catalogid, has_sourceid.
#'   }
#' @export
get_histogram_mdb_text_columns <- function(conn, module) {
  query <- sprintf("
   WITH t AS (
     SELECT '%s' table_name
   ),
   columns_to_histogram AS (
     SELECT
       c.table_name,
       c.column_name,
       c.udt_name
     FROM information_schema.columns c
     JOIN t USING(table_name)
     WHERE c.column_name !~ 'runid|catalogid|sourceid|fstate|sostype|error|other|file_id|transfer_id'
       AND c.udt_name IN ('text', 'varchar', 'bpchar', 'name', 'bool')
       AND c.table_schema in (current_schema(),current_schema()||'_mdb')
     ORDER BY c.table_name, c.column_name
   )
   SELECT * FROM columns_to_histogram
 ", module)

  raw_df <- dbGetQuery(conn, query)
  if (nrow(raw_df) > 0) {
    raw_df$col_ref <- raw_df$column_name
    raw_df$min_array_len <- 0L
    raw_df$array_col <- NA_character_
  } else {
    raw_df$col_ref <- character(0)
    raw_df$min_array_len <- integer(0)
    raw_df$array_col <- NA_character_
  }
  table_info <- detect_system_columns(conn, unique(raw_df$table_name))
  list(columns = raw_df, table_info = table_info)
}


#' Build Global Statistics Query Using CTE + UNION ALL
#'
#' Generates a SQL query that computes min, max, NaN count, and valid count
#' for all numeric columns in a table. Uses a CTE for the base data and
#' UNION ALL to produce one row per column (long format), avoiding PostgreSQL's
#' 1664 target list limit that occurs with wide-format queries on tables with
#' many array-expanded columns.
#'
#' @param table_name Name of the table to query
#' @param columns_df Data frame of columns (from get_histogram_columns)
#' @param runid Run ID to filter data
#' @param join_clause Optional SQL JOIN clause to limit selection
#' @param table_alias Alias for the main table when using join_clause (default "t")
#' @param table_info Data frame with has_runid/has_catalogid/has_sourceid flags (from detect_system_columns)
#' @return SQL query string, or NULL if no columns found
#' @keywords internal
build_global_stats_query <- function(table_name, columns_df, runid,
                                     join_clause = NULL, table_alias = "t",
                                     table_info = NULL, group_key = NULL) {

  table_cols <- columns_df[columns_df$table_name == table_name, ]

  if (nrow(table_cols) == 0) return(NULL)

  # Determine which system columns exist for this table
  has_runid <- TRUE
  has_catalogid <- TRUE
  has_sourceid <- TRUE
  if (!is.null(table_info)) {
    ti <- table_info[table_info$table_name == table_name, ]
    if (nrow(ti) > 0) {
      has_runid <- ti$has_runid[1]
      has_catalogid <- ti$has_catalogid[1]
      has_sourceid <- ti$has_sourceid[1]
    }
  }

  # Build FROM clause
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

  # Build column list for CTE — include all columns needed for stats
  cte_parts <- sapply(seq_len(nrow(table_cols)), function(i) {
    ref_expr <- paste0(col_prefix, table_cols$col_ref[i])
    col_name <- table_cols$column_name[i]
    if (table_cols$col_ref[i] != col_name) {
      sprintf("%s AS %s", ref_expr, col_name)
    } else {
      ref_expr
    }
  })

  # Ensure cardinality columns are in CTE for array element filtering
  # array_len_ref holds the raw expression whose cardinality we need
  # (e.g., "fouriercoefficients" or "(modelresult).periods")
  array_len_rows <- table_cols[table_cols$min_array_len > 0 & !is.na(table_cols$array_col), ]
  if (nrow(array_len_rows) > 0) {
    len_needed <- unique(array_len_rows[, c("array_col", "array_len_ref"), drop = FALSE])
    for (j in seq_len(nrow(len_needed))) {
      ac <- len_needed$array_col[j]
      len_name <- paste0(ac, "_len")
      if (!(len_name %in% table_cols$column_name)) {
        len_ref <- len_needed$array_len_ref[j]
        if (is.na(len_ref) || !nzchar(len_ref)) {
          len_ref <- ac
        }
        cte_parts <- c(cte_parts,
                        sprintf("cardinality(%s%s) AS %s", col_prefix, len_ref, len_name))
      }
    }
  }
  # Add group_key expression to CTE if specified
  # Wrap in parentheses to prevent partParalXZ4 regex from mangling column names
  # that start with table name patterns (e.g. timeseriesresulttypename contains
  # 'timeseriesresult' which the partitioning regex would replace)
  has_group_key <- !is.null(group_key) && nzchar(group_key)
  if (has_group_key) {
    cte_parts <- c(cte_parts, sprintf("(%s%s) AS group_key", col_prefix, group_key))
  }
  col_list <- paste(cte_parts, collapse = ", ")

  # Build WHERE clause conditionally based on available system columns
  where_parts <- c()
  if (has_runid) {
    where_parts <- c(where_parts, sprintf("%s = %d", runid_ref, runid))
  }
  if (has_catalogid) {
    where_parts <- c(where_parts, "catalogid=getmaincatalog()")
  }
  if (has_sourceid) {
    # sourceid = sourceid construct for partParalXZ4 parallel execution
    where_parts <- c(where_parts, sprintf("%s = %s", sourceid_ref, sourceid_ref))
  }

  if (length(where_parts) > 0) {
    cte <- sprintf("WITH base AS (\n SELECT %s\n FROM %s\n WHERE %s\n)",
                   col_list, from_clause, paste(where_parts, collapse = " AND "))
  } else {
    cte <- sprintf("WITH base AS (\n SELECT %s\n FROM %s\n)",
                   col_list, from_clause)
  }

  # SQL fragments for group_key support
  gk_select <- if (has_group_key) "\n   group_key," else ""
  gk_group_by <- if (has_group_key) "\n GROUP BY group_key" else ""

  # Build UNION ALL of stats queries — one per column (long format output)
  # This avoids PG's 1664 target list limit that wide-format hits with many columns
  union_parts <- sapply(seq_len(nrow(table_cols)), function(i) {
    col <- table_cols[i, ]
    col_name <- col$column_name
    col_ref <- col_name  # Already aliased in CTE

    # Build cardinality filter for array element columns
    extra_where <- ""
    if (!is.na(col$array_col) && col$min_array_len > 0) {
      extra_where <- sprintf("%s_len >= %d AND ", col$array_col, col$min_array_len)
    }

    if (grepl("^float", col$udt_name)) {
      sprintf("
 SELECT%s
   '%s'::TEXT AS column_name,
   COALESCE(min(%s) FILTER (WHERE %s%s IS NOT NULL AND %s != 'NaN'::float8 AND %s != 'Infinity'::float8 AND %s != '-Infinity'::float8), 'NaN'::float8)::NUMERIC AS global_min,
   COALESCE(max(%s) FILTER (WHERE %s%s IS NOT NULL AND %s != 'NaN'::float8 AND %s != 'Infinity'::float8 AND %s != '-Infinity'::float8), 'NaN'::float8)::NUMERIC AS global_max,
   COALESCE(count(*) FILTER (WHERE %s%s = 'NaN'::float8), 0)::BIGINT AS nan_count,
   COALESCE(count(*) FILTER (WHERE %s(%s = 'Infinity'::float8 OR %s = '-Infinity'::float8)), 0)::BIGINT AS inf_count,
   COALESCE(count(*) FILTER (WHERE %s%s IS NOT NULL AND %s != 'NaN'::float8 AND %s != 'Infinity'::float8 AND %s != '-Infinity'::float8), 0)::BIGINT AS non_nan_count,
   count(*)::BIGINT AS total_count
 FROM base%s",
              gk_select,
              col_name,
              col_ref, extra_where, col_ref, col_ref, col_ref, col_ref,
              col_ref, extra_where, col_ref, col_ref, col_ref, col_ref,
              extra_where, col_ref,
              extra_where, col_ref, col_ref,
              extra_where, col_ref, col_ref, col_ref, col_ref,
              gk_group_by)
    } else {
      sprintf("
 SELECT%s
   '%s'::TEXT AS column_name,
   COALESCE(min(%s) FILTER (WHERE %s%s IS NOT NULL), 0)::NUMERIC AS global_min,
   COALESCE(max(%s) FILTER (WHERE %s%s IS NOT NULL), 0)::NUMERIC AS global_max,
   0::BIGINT AS nan_count,
   0::BIGINT AS inf_count,
   COALESCE(count(*) FILTER (WHERE %s%s IS NOT NULL), 0)::BIGINT AS non_nan_count,
   count(*)::BIGINT AS total_count
 FROM base%s",
              gk_select,
              col_name,
              col_ref, extra_where, col_ref,
              col_ref, extra_where, col_ref,
              extra_where, col_ref,
              gk_group_by)
    }
  })

  # Wrap in final query with table_name column
  gk_final_select <- if (has_group_key) "\n group_key," else ""
  query <- sprintf("%s
SELECT
 '%s'::TEXT AS table_name,%s
 column_name,
 global_min,
 global_max,
 nan_count,
 inf_count,
 non_nan_count,
 total_count
FROM (
%s
) all_columns",
                   cte,
                   table_name,
                   gk_final_select,
                   paste(union_parts, collapse = "\n UNION ALL\n"))

  return(query)
}

#' Build Aggregation Query for Partial Global Statistics
#'
#' Generates a SQL query to aggregate partial global statistics results from
#' parallel execution. Aggregates long-format rows (one per column per chunk)
#' by taking min of mins, max of maxes, and sum of counts.
#'
#' @param partial_table_name Name of the table containing partial results
#' @return SQL query string
#' @keywords internal
build_global_stats_aggregation_query <- function(partial_table_name, group_key = NULL) {
  has_gk <- !is.null(group_key) && nzchar(group_key)
  gk_select <- if (has_gk) "\n group_key," else ""
  gk_group <- if (has_gk) ", group_key" else ""

  sprintf("
SELECT
 table_name,%s
 column_name,
 min(NULLIF(global_min, 'NaN'::NUMERIC))::NUMERIC AS global_min,
 max(NULLIF(global_max, 'NaN'::NUMERIC))::NUMERIC AS global_max,
 COALESCE(sum(nan_count), 0)::BIGINT AS nan_count,
 COALESCE(sum(inf_count), 0)::BIGINT AS inf_count,
 COALESCE(sum(non_nan_count), 0)::BIGINT AS non_nan_count,
 COALESCE(sum(total_count), 0)::BIGINT AS total_count
FROM %s
GROUP BY table_name, column_name%s
ORDER BY table_name, column_name%s",
          gk_select, partial_table_name, gk_group, gk_group)
}

#' Sanitize Statistics Data Frame
#'
#' Converts integer64 values to numeric and sanitizes Inf/NaN/NA values
#' in global statistics results. Used after both parallel and direct execution.
#'
#' @param stats_df Data frame with columns: table_name, column_name,
#'   global_min, global_max, nan_count, inf_count, non_nan_count
#' @return Sanitized data frame
#' @keywords internal
sanitize_stats_df <- function(stats_df) {
  if (nrow(stats_df) == 0) return(stats_df)

  # Convert integer64 to numeric
  stats_df <- stats_df %>%
    mutate(across(where(bit64::is.integer64), as.numeric))

  # Sanitize numeric values
  sanitize_value <- function(x, default = NA_real_) {
    x <- as.numeric(x)
    ifelse(is.na(x) | is.nan(x) | is.infinite(x), default, x)
  }

  stats_df %>%
    mutate(
      global_min = sanitize_value(global_min),
      global_max = sanitize_value(global_max),
      nan_count = sanitize_value(nan_count, default = 0),
      inf_count = sanitize_value(inf_count, default = 0),
      non_nan_count = sanitize_value(non_nan_count, default = 0),
      total_count = sanitize_value(total_count, default = 0)
    )
}

#' @describeIn pivot_stats_to_long Kept for backward compatibility
#' @keywords internal
pivot_stats_to_long <- function(stats_wide, table_name, columns_df) {
  table_cols <- columns_df[columns_df$table_name == table_name, ]

  # Helper to sanitize numeric values: replace Inf/-Inf/NaN with NA

  sanitize_value <- function(x, default = NA_real_) {
    x <- as.numeric(x)
    if (is.null(x) || length(x) == 0 || is.na(x) || is.nan(x) || is.infinite(x)) {
      return(default)
    }
    return(x)
  }

  stats_long <- lapply(seq_len(nrow(table_cols)), function(i) {
    col_name <- table_cols$column_name[i]

    raw_min <- stats_wide[[paste0(col_name, "_min")]]
    raw_max <- stats_wide[[paste0(col_name, "_max")]]
    raw_nan <- stats_wide[[paste0(col_name, "_nan")]]
    raw_inf <- stats_wide[[paste0(col_name, "_inf")]]
    raw_valid <- stats_wide[[paste0(col_name, "_valid")]]

    data.frame(
      table_name = table_name,
      column_name = col_name,
      global_min = sanitize_value(raw_min),
      global_max = sanitize_value(raw_max),
      nan_count = sanitize_value(raw_nan, default = 0),
      inf_count = sanitize_value(raw_inf, default = 0),
      non_nan_count = sanitize_value(raw_valid, default = 0),
      stringsAsFactors = FALSE
    )
  })

  bind_rows(stats_long)
}

#' Compute Global Statistics for All Tables (Parallel Execution)
#'
#' Executes statistics queries for all tables in the module using parallel
#' execution via partParalXZ4, then aggregates partial results to compute
#' global min/max and NaN counts for each numeric column.
#'
#' @param conn DBI database connection
#' @param columns_df Data frame of columns (from get_histogram_columns)
#' @param runid Run ID to filter data
#' @param join_clauses Named list of table-specific JOIN clauses
#' @param default_join_clause Default JOIN clause for tables not in join_clauses
#' @param db_user Database user for parallel execution
#' @param schema Output schema for temporary stats tables (default "dr4_ops_cs48_mv")
#' @param slack_user Slack user for notifications (default "@nienarto")
#' @param parallelism Number of parallel workers (default 80)
#' @param num_chunks Number of data chunks (default 600)
#' @param execute If TRUE, execute in parallel; if FALSE, run directly (default TRUE)
#' @param debug If TRUE, print detailed debug output
#' @return Data frame with global statistics for all columns
#' @keywords internal
compute_global_stats <- function(conn, columns_df, runid,
                                 join_clauses = NULL, default_join_clause = NULL,
                                 db_user = NULL, schema = "dr4_ops_cs48_mv",
                                 slack_user = "@nienarto", parallelism = 80,
                                 num_chunks = 600, execute = TRUE, debug = FALSE,
                                 table_info = NULL, group_key = NULL) {

  if (nrow(columns_df) == 0) {
    stop("No columns found for histogram generation")
  }

  tables <- unique(columns_df$table_name)

  cat(sprintf("Computing global stats for %d tables, %d columns (parallel execution)...\n",
              length(tables), nrow(columns_df)))

  # Batch size for UNION ALL queries: limits SQL string length to avoid

  # xargs "argument line too long" inside partParalXZ4
  max_stats_columns <- 50

  all_stats <- list()

  for (tbl in tables) {
    if (!is.null(join_clauses) && tbl %in% names(join_clauses)) {
      join_clause <- join_clauses[[tbl]]
    } else {
      join_clause <- default_join_clause
    }

    # Check if this table should use parallel execution
    tbl_use_parallel <- TRUE
    tbl_has_sourceid <- TRUE
    if (!is.null(table_info)) {
      ti <- table_info[table_info$table_name == tbl, ]
      if (nrow(ti) > 0) {
        tbl_has_sourceid <- ti$has_sourceid[1]
        tbl_use_parallel <- if ("use_parallel" %in% names(ti)) isTRUE(ti$use_parallel[1]) else tbl_has_sourceid
      }
    }

    tbl_cols <- columns_df[columns_df$table_name == tbl, ]
    n_cols <- nrow(tbl_cols)
    cat(sprintf("  %s (%d columns)...\n", tbl, n_cols))

    # Split into batches to keep SQL query length within xargs limits
    if (n_cols > max_stats_columns) {
      n_batches <- ceiling(n_cols / max_stats_columns)
      cat(sprintf("    Splitting into %d batches of ~%d columns\n", n_batches, max_stats_columns))
      batch_indices <- split(seq_len(n_cols), ceiling(seq_len(n_cols) / max_stats_columns))
    } else {
      batch_indices <- list(seq_len(n_cols))
    }

    for (b in seq_along(batch_indices)) {
      batch_col_names <- tbl_cols$column_name[batch_indices[[b]]]
      batch_columns_df <- columns_df %>%
        filter(table_name == tbl, column_name %in% batch_col_names)

      query <- build_global_stats_query(tbl, batch_columns_df, runid, join_clause,
                                         table_info = table_info, group_key = group_key)
      if (is.null(query)) next

      batch_n <- nrow(batch_columns_df)
      batch_label <- if (length(batch_indices) > 1) sprintf(" [batch %d/%d, %d cols]", b, length(batch_indices), batch_n) else ""

      if (execute && !is.null(db_user) && tbl_use_parallel) {
        # Execute via parallel script
        batch_suffix <- if (length(batch_indices) > 1) sprintf("_b%d", b) else ""
        output_table <- sprintf("%s.stats_%s%s_%d", schema,
                                 sanitize_identifier(tbl, 45), batch_suffix, runid)

        if (debug) {
          cat(sprintf("    Output table: %s\n", output_table))
          cat(sprintf("    Query length: %d chars%s\n", nchar(query), batch_label))
        }

        exit_code <- execute_parallel_script(
          runid = runid,
          output_table = output_table,
          sql_query = query,
          db_user = db_user,
          slack_user = slack_user,
          parallelism = parallelism,
          num_chunks = num_chunks,
          description = sprintf("GlobalStats %s%s", tbl, batch_label)
        )

        if (exit_code != 0) {
          warning(sprintf("Parallel stats query for %s%s failed with exit code %d", tbl, batch_label, exit_code))
          next
        }

        # Aggregate partial results from parallel execution (long format)
        agg_query <- build_global_stats_aggregation_query(output_table, group_key = group_key)
        if (debug) {
          cat(sprintf("    Aggregation query: %s\n", agg_query))
        }
        stats_long <- tryCatch({
          dbGetQuery(conn, agg_query)
        }, error = function(e) {
          warning(sprintf("Aggregation query for %s%s failed: %s", tbl, batch_label, e$message))
          NULL
        })

        # Drop the temporary partial results table
        tryCatch({
          dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", output_table))
          if (debug) cat(sprintf("    Dropped temporary table: %s\n", output_table))
        }, error = function(e) {
          warning(sprintf("Could not drop temporary table %s: %s", output_table, e$message))
        })

        if (is.null(stats_long)) next

      } else {
        # Execute directly (non-parallel: testing, small datasets, or table without sourceid)
        # Remove the sourceid = sourceid clause for direct execution (if present)
        direct_query <- gsub(" AND [a-z_]*\\.?sourceid = [a-z_]*\\.?sourceid", "", query)
        if (debug) {
          if (!tbl_has_sourceid) {
            cat(sprintf("    Table %s has no sourceid - executing directly\n", tbl))
          } else if (!tbl_use_parallel) {
            cat(sprintf("    Table %s below parallel threshold - executing directly\n", tbl))
          }
          cat(sprintf("    Direct query (first 500 chars): %s...\n", substr(direct_query, 1, 500)))
        }
        stats_long <- tryCatch({
          dbGetQuery(conn, direct_query)
        }, error = function(e) {
          warning(sprintf("Direct stats query for %s%s failed: %s", tbl, batch_label, e$message))
          NULL
        })
        if (is.null(stats_long)) next
      }

      # Sanitize stats (integer64 conversion, NaN/Inf handling)
      stats_long <- sanitize_stats_df(stats_long)
      batch_key <- if (length(batch_indices) > 1) sprintf("%s_b%d", tbl, b) else tbl
      all_stats[[batch_key]] <- stats_long
    }
  }

  global_stats <- bind_rows(all_stats)
  if (nrow(global_stats) == 0) {
    cat("WARNING: No stats computed — all queries failed or returned empty results\n")
    # Return empty data frame with expected columns so downstream doesn't crash
    global_stats <- data.frame(
      table_name = character(0), column_name = character(0),
      global_min = numeric(0), global_max = numeric(0),
      nan_count = numeric(0), inf_count = numeric(0),
      non_nan_count = numeric(0), total_count = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  cat(sprintf("Computed stats for %d columns\n", nrow(global_stats)))

  return(global_stats)
}

#' Build Single Column Bucketing SELECT Statement
#'
#' Generates a SELECT statement for computing histogram buckets for a single column,
#' using precomputed global min/max boundaries. Used as part of UNION ALL query.
#' For float columns, filters out NaN and Inf values to prevent NUMERIC cast errors.
#' For integer columns, caps the bucket count at the distinct value range and uses
#' half-integer boundaries so each integer falls cleanly into one bucket.
#'
#' @param column_name Name of the column
#' @param udt_name PostgreSQL data type (float8, int4, etc.)
#' @param global_min Precomputed global minimum value
#' @param global_max Precomputed global maximum value
#' @param nan_count Precomputed count of NaN values
#' @param inf_count Precomputed count of Inf/-Inf values
#' @param non_nan_count Precomputed count of valid (non-NaN, non-Inf) values
#' @param num_buckets Number of histogram buckets
#' @param col_ref Column reference (may include table alias prefix)
#' @param table_name Name of the source table
#' @param extra_where Additional WHERE clause (e.g., cardinality filter for array elements)
#' @return SQL SELECT statement string
#' @keywords internal
build_column_bucket_select <- function(column_name, udt_name, global_min, global_max,
                                       nan_count, inf_count, non_nan_count, num_buckets,
                                       col_ref, table_name, extra_where = "",
                                       group_key_value = NULL, total_count = 0) {

  # Sanitize inputs — use as.numeric (not as.integer) to avoid overflow on large counts
  nan_count <- as.numeric(ifelse(is.na(nan_count) | is.nan(nan_count), 0, nan_count))
  inf_count <- as.numeric(ifelse(is.na(inf_count) | is.nan(inf_count), 0, inf_count))
  non_nan_count <- as.numeric(ifelse(is.na(non_nan_count) | is.nan(non_nan_count), 0, non_nan_count))
  total_count <- as.numeric(ifelse(is.na(total_count) | is.nan(total_count), 0, total_count))

  is_float <- grepl("^float", udt_name)
  is_int <- grepl("^int", udt_name)

  # Check for invalid min/max - safety net
  if (is.na(global_min) || is.na(global_max) ||
      is.infinite(global_min) || is.infinite(global_max) ||
      is.nan(global_min) || is.nan(global_max)) {
    warning(sprintf("Invalid stats for column %s: min=%s, max=%s - using fallback",
                    column_name, global_min, global_max))
    global_min <- 0
    global_max <- 1
  }

  # For integer columns: use one bucket per distinct value when the range is
  # small enough (up to 500), otherwise fall back to num_buckets.
  # Half-integer boundaries ensure each integer falls cleanly into one bucket.
  if (is_int) {
    int_range <- as.numeric(global_max) - as.numeric(global_min) + 1
    if (int_range <= 500) {
      effective_buckets <- int_range
    } else {
      effective_buckets <- num_buckets
    }
    bucket_lo <- as.numeric(global_min) - 0.5
    bucket_hi <- as.numeric(global_max) + 0.5
  } else {
    effective_buckets <- num_buckets
    bucket_lo <- global_min
    bucket_hi <- global_max
  }

  # WHERE clause depends on column type
  if (is_float) {
    where_filter <- sprintf(
      "%s IS NOT NULL AND %s != 'NaN'::float8 AND %s != 'Infinity'::float8 AND %s != '-Infinity'::float8",
      col_ref, col_ref, col_ref, col_ref)
  } else {
    where_filter <- sprintf("%s IS NOT NULL", col_ref)
  }

  # Prepend extra WHERE condition (e.g., cardinality check for array elements)
  if (nzchar(extra_where)) {
    where_filter <- paste(extra_where, "AND", where_filter)
  }

  # Group key support: add group_key filter and output column
  has_gk <- !is.null(group_key_value)
  if (has_gk) {
    gk_select <- sprintf("\n   '%s'::TEXT AS group_key,", gsub("'", "''", group_key_value))
    gk_where <- sprintf("group_key = '%s' AND ", gsub("'", "''", group_key_value))
    where_filter <- paste0(gk_where, where_filter)
  } else {
    gk_select <- ""
  }

  # Handle edge case: all values are the same (or all NULL/NaN)
  if (global_min >= global_max) {

    safe_min <- ifelse(is.na(global_min), 0, global_min)
    safe_max <- ifelse(is.na(global_max), 0, global_max)

    select_expr <- sprintf("
 SELECT%s
   '%s'::TEXT AS column_name,
   1 AS bucket,
   count(*)::BIGINT AS freq,
   min(%s)::NUMERIC AS bucket_min,
   max(%s)::NUMERIC AS bucket_max,
   avg(%s)::NUMERIC AS bucket_avg,
   %.17g::NUMERIC AS global_min,
   %.17g::NUMERIC AS global_max,
   %.0f::BIGINT AS nan_count,
   %.0f::BIGINT AS inf_count,
   %.0f::BIGINT AS non_nan_count,
   %.0f::BIGINT AS total_count
 FROM base
 WHERE %s",
                           gk_select,
                           column_name,
                           col_ref, col_ref, col_ref,
                           safe_min, safe_max,
                           nan_count, inf_count, non_nan_count, total_count,
                           where_filter)
  } else {
    # Normal case: use width_bucket with fixed boundaries
    select_expr <- sprintf("
 SELECT%s
   '%s'::TEXT AS column_name,
   width_bucket(%s, %.17g::float8, %.17g::float8, %d) AS bucket,
   count(*)::BIGINT AS freq,
   min(%s)::NUMERIC AS bucket_min,
   max(%s)::NUMERIC AS bucket_max,
   avg(%s)::NUMERIC AS bucket_avg,
   %.17g::NUMERIC AS global_min,
   %.17g::NUMERIC AS global_max,
   %.0f::BIGINT AS nan_count,
   %.0f::BIGINT AS inf_count,
   %.0f::BIGINT AS non_nan_count,
   %.0f::BIGINT AS total_count
 FROM base
 WHERE %s
 GROUP BY width_bucket(%s, %.17g::float8, %.17g::float8, %d)",
                           gk_select,
                           column_name,
                           col_ref, bucket_lo, bucket_hi, effective_buckets,
                           col_ref, col_ref, col_ref,
                           global_min, global_max,
                           nan_count, inf_count, non_nan_count, total_count,
                           where_filter,
                           col_ref, bucket_lo, bucket_hi, effective_buckets)
  }

  return(select_expr)
}

#' Build Table Histogram Query
#'
#' Generates a complete SQL query for computing histograms for all columns
#' in a table using a CTE and UNION ALL pattern. Uses precomputed global
#' min/max boundaries to ensure consistent bucketing across parallel chunks.
#' Conditionally includes runid/catalogid/sourceid filters based on table_info.
#'
#' @param table_name Name of the table to query
#' @param columns_df Data frame of columns (from get_histogram_columns)
#' @param global_stats Data frame of global statistics (from compute_global_stats)
#' @param runid Run ID to filter data
#' @param num_buckets Number of histogram buckets (default 20)
#' @param join_clause Optional SQL JOIN clause to limit selection
#' @param table_alias Alias for the main table when using join_clause (default "t")
#' @param table_info Data frame with has_runid/has_catalogid/has_sourceid flags (from detect_system_columns)
#' @return SQL query string, or NULL if no columns found
#' @keywords internal
build_table_histogram_query <- function(table_name, columns_df, global_stats, runid,
                                        num_buckets = 20, join_clause = NULL,
                                        table_alias = "t", table_info = NULL,
                                        group_key = NULL) {

  table_cols <- columns_df[columns_df$table_name == table_name, ]
  table_stats <- global_stats[global_stats$table_name == table_name, ]

  if (nrow(table_cols) == 0) return(NULL)

  # Determine which system columns exist for this table
  has_runid <- TRUE
  has_catalogid <- TRUE
  has_sourceid <- TRUE
  if (!is.null(table_info)) {
    ti <- table_info[table_info$table_name == table_name, ]
    if (nrow(ti) > 0) {
      has_runid <- ti$has_runid[1]
      has_catalogid <- ti$has_catalogid[1]
      has_sourceid <- ti$has_sourceid[1]
    }
  }

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

  # Build column list for CTE, aliasing array element expressions
  cte_parts <- sapply(seq_len(nrow(table_cols)), function(i) {
    ref_expr <- paste0(col_prefix, table_cols$col_ref[i])
    col_name <- table_cols$column_name[i]
    if (table_cols$col_ref[i] != col_name) {
      # Array element or derived expression: need explicit alias
      sprintf("%s AS %s", ref_expr, col_name)
    } else {
      ref_expr
    }
  })

  # Ensure cardinality columns are in CTE for array element filtering
  # (even if _len column itself was filtered out by stats validation)
  # array_len_ref holds the raw expression whose cardinality we need
  array_len_rows <- table_cols[table_cols$min_array_len > 0 & !is.na(table_cols$array_col), ]
  if (nrow(array_len_rows) > 0) {
    len_needed <- unique(array_len_rows[, c("array_col", "array_len_ref"), drop = FALSE])
    for (j in seq_len(nrow(len_needed))) {
      ac <- len_needed$array_col[j]
      len_name <- paste0(ac, "_len")
      if (!(len_name %in% table_cols$column_name)) {
        len_ref <- len_needed$array_len_ref[j]
        if (is.na(len_ref) || !nzchar(len_ref)) {
          len_ref <- ac
        }
        cte_parts <- c(cte_parts,
                        sprintf("cardinality(%s%s) AS %s", col_prefix, len_ref, len_name))
      }
    }
  }
  # Add group_key expression to CTE if specified
  # Wrap in parentheses to prevent partParalXZ4 regex mangling (see build_global_stats_query)
  has_group_key <- !is.null(group_key) && nzchar(group_key)
  if (has_group_key) {
    cte_parts <- c(cte_parts, sprintf("(%s%s) AS group_key", col_prefix, group_key))
  }
  col_list <- paste(cte_parts, collapse = ", ")

  # Build CTE with conditional WHERE based on available system columns
  where_parts <- c()
  if (has_runid) {
    where_parts <- c(where_parts, sprintf("%s = %d", runid_ref, runid))
  }
  if (has_catalogid) {
    where_parts <- c(where_parts, "catalogid = getmaincatalog()")
  }
  if (has_sourceid) {
    where_parts <- c(where_parts, sprintf("%s = %s", sourceid_ref, sourceid_ref))
  }

  if (length(where_parts) > 0) {
    cte <- sprintf("WITH base AS (\n SELECT %s\n FROM %s\n WHERE %s\n)",
                   col_list, from_clause, paste(where_parts, collapse = " AND "))
  } else {
    cte <- sprintf("WITH base AS (\n SELECT %s\n FROM %s\n)",
                   col_list, from_clause)
  }

  # Build UNION ALL of bucket queries for each column (and each group_key value)
  union_parts_list <- list()
  for (i in seq_len(nrow(table_cols))) {
    col <- table_cols[i, ]

    # Build cardinality filter for array element columns
    # In the CTE, array length is available as <array_col>_len
    extra_where <- ""
    if (!is.na(col$array_col) && col$min_array_len > 0) {
      extra_where <- sprintf("%s_len >= %d", col$array_col, col$min_array_len)
    }

    if (has_group_key) {
      # Per-group bucketing: iterate over distinct group_key values
      col_stats <- table_stats[table_stats$column_name == col$column_name, ]
      if (nrow(col_stats) == 0) next
      for (g in seq_len(nrow(col_stats))) {
        stat <- col_stats[g, ]
        gk_val <- as.character(stat$group_key)
        union_parts_list[[length(union_parts_list) + 1]] <- build_column_bucket_select(
          column_name = col$column_name,
          udt_name = col$udt_name,
          global_min = stat$global_min,
          global_max = stat$global_max,
          nan_count = stat$nan_count,
          inf_count = stat$inf_count,
          non_nan_count = stat$non_nan_count,
          num_buckets = num_buckets,
          col_ref = col$column_name,
          table_name = table_name,
          extra_where = extra_where,
          group_key_value = gk_val,
          total_count = stat$total_count
        )
      }
    } else {
      stat <- table_stats[table_stats$column_name == col$column_name, ]
      union_parts_list[[length(union_parts_list) + 1]] <- build_column_bucket_select(
        column_name = col$column_name,
        udt_name = col$udt_name,
        global_min = stat$global_min,
        global_max = stat$global_max,
        nan_count = stat$nan_count,
        inf_count = stat$inf_count,
        non_nan_count = stat$non_nan_count,
        num_buckets = num_buckets,
        col_ref = col$column_name,
        table_name = table_name,
        extra_where = extra_where,
        total_count = stat$total_count
      )
    }
  }

  if (length(union_parts_list) == 0) return(NULL)

  # Combine into final query
  gk_final_select <- if (has_group_key) "\n group_key," else ""
  query <- sprintf("%s
SELECT
 '%s'::TEXT AS table_name,%s
 column_name,
 bucket,
 freq,
 bucket_min,
 bucket_max,
 bucket_avg,
 global_min,
 global_max,
 nan_count,
 inf_count,
 non_nan_count,
 total_count
FROM (
%s
) all_columns",
                   cte,
                   table_name,
                   gk_final_select,
                   paste(union_parts_list, collapse = "\n UNION ALL\n"))

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
build_histogram_aggregation_query <- function(partial_table_name, group_key = NULL) {
  has_gk <- !is.null(group_key) && nzchar(group_key)
  gk_select <- if (has_gk) "\n group_key," else ""
  gk_group <- if (has_gk) ", group_key" else ""

  sprintf("
SELECT
 table_name,%s
 column_name,
 bucket,
 SUM(freq)::NUMERIC AS freq,
 MIN(bucket_min)::NUMERIC AS bucket_min,
 MAX(bucket_max)::NUMERIC AS bucket_max,
 SUM(bucket_avg * freq) / NULLIF(SUM(freq), 0) AS bucket_avg,
 MIN(global_min)::NUMERIC AS global_min,
 MAX(global_max)::NUMERIC AS global_max,
 MAX(nan_count)::NUMERIC AS nan_count,
 MAX(inf_count)::NUMERIC AS inf_count,
 MAX(non_nan_count)::NUMERIC AS non_nan_count,
 MAX(total_count)::NUMERIC AS total_count
FROM %s
GROUP BY table_name, column_name, bucket%s
ORDER BY table_name, column_name%s, bucket",
          gk_select, partial_table_name, gk_group, gk_group)
}

#' Build Categorical Histogram Query for a Single Table
#'
#' Generates a SQL query that computes value frequency counts for all text
#' columns in a table using a CTE and UNION ALL pattern. Used for categorical
#' histogram generation via parallel execution.
#'
#' @param table_name Name of the table to query
#' @param columns_df Data frame of text columns
#' @param runid Run ID to filter data
#' @param join_clause Optional SQL JOIN clause
#' @param table_alias Alias for the main table when using join_clause (default "t")
#' @param table_info Data frame with has_runid/has_catalogid/has_sourceid flags
#' @return SQL query string, or NULL if no columns found
#' @keywords internal
build_categorical_histogram_query <- function(table_name, columns_df, runid,
                                              join_clause = NULL, table_alias = "t",
                                              table_info = NULL, group_key = NULL) {

  table_cols <- columns_df[columns_df$table_name == table_name, ]
  if (nrow(table_cols) == 0) return(NULL)

  # Determine which system columns exist for this table
  has_runid <- TRUE
  has_catalogid <- TRUE
  has_sourceid <- TRUE
  if (!is.null(table_info)) {
    ti <- table_info[table_info$table_name == table_name, ]
    if (nrow(ti) > 0) {
      has_runid <- ti$has_runid[1]
      has_catalogid <- ti$has_catalogid[1]
      has_sourceid <- ti$has_sourceid[1]
    }
  }

  # Build FROM clause
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
  cte_col_parts <- sprintf("%s%s", col_prefix, table_cols$column_name)
  # Wrap in parentheses to prevent partParalXZ4 regex mangling (see build_global_stats_query)
  has_group_key <- !is.null(group_key) && nzchar(group_key)
  if (has_group_key) {
    cte_col_parts <- c(cte_col_parts, sprintf("(%s%s) AS group_key", col_prefix, group_key))
  }
  cte_cols <- paste(cte_col_parts, collapse = ", ")

  # Build WHERE clause conditionally based on available system columns
  where_parts <- c()
  if (has_runid) {
    where_parts <- c(where_parts, sprintf("%s = %d", runid_ref, runid))
  }
  if (has_catalogid) {
    where_parts <- c(where_parts, "catalogid = getmaincatalog()")
  }
  if (has_sourceid) {
    where_parts <- c(where_parts, sprintf("%s = %s", sourceid_ref, sourceid_ref))
  }

  if (length(where_parts) > 0) {
    cte <- sprintf("WITH base AS (\n SELECT %s\n FROM %s\n WHERE %s\n)",
                   cte_cols, from_clause, paste(where_parts, collapse = " AND "))
  } else {
    cte <- sprintf("WITH base AS (\n SELECT %s\n FROM %s\n)",
                   cte_cols, from_clause)
  }

  # SQL fragments for group_key support
  gk_select <- if (has_group_key) "\n   group_key," else ""
  gk_group_by <- if (has_group_key) ", group_key" else ""

  # Build UNION ALL of frequency queries for each column
  union_parts <- sapply(seq_len(nrow(table_cols)), function(i) {
    col_name <- table_cols$column_name[i]
    sprintf("
 SELECT%s
   '%s'::TEXT AS column_name,
   %s::TEXT AS category_value,
   COUNT(*)::BIGINT AS freq,
   0::BIGINT AS null_count
 FROM base
 WHERE %s IS NOT NULL
 GROUP BY %s%s
 UNION ALL
 SELECT%s
   '%s'::TEXT AS column_name,
   '__NULL__'::TEXT AS category_value,
   0::BIGINT AS freq,
   COALESCE(COUNT(*) FILTER (WHERE %s IS NULL), 0)::BIGINT AS null_count
 FROM base%s",
            gk_select, col_name, col_name, col_name, col_name, gk_group_by,
            gk_select, col_name, col_name,
            if (has_group_key) "\n GROUP BY group_key" else "")
  })

  # Combine into final query
  gk_final_select <- if (has_group_key) "\n group_key," else ""
  query <- sprintf("%s
SELECT
 '%s'::TEXT AS table_name,%s
 column_name,
 category_value,
 freq,
 null_count
FROM (
%s
) all_columns",
                   cte,
                   table_name,
                   gk_final_select,
                   paste(union_parts, collapse = "\n UNION ALL\n"))

  return(query)
}

#' Build Aggregation Query for Categorical Histogram Results
#'
#' Generates a SQL query to aggregate categorical histogram results from
#' parallel execution, summing frequencies across chunks.
#'
#' @param partial_table_name Name of the table containing partial results
#' @return SQL query string
#' @keywords internal
build_categorical_aggregation_query <- function(partial_table_name, group_key = NULL) {
  has_gk <- !is.null(group_key) && nzchar(group_key)
  gk_select <- if (has_gk) "\n group_key," else ""
  gk_group <- if (has_gk) ", group_key" else ""

  sprintf("
SELECT
 table_name,%s
 column_name,
 category_value,
 SUM(freq)::BIGINT AS freq,
 MAX(null_count)::BIGINT AS null_count
FROM %s
GROUP BY table_name, column_name, category_value%s
ORDER BY table_name, column_name%s, freq DESC",
          gk_select, partial_table_name, gk_group, gk_group)
}

#' Collapse Categorical Histogram to Top N Values
#'
#' For each (table_name, column_name) group, keeps the top \code{max_categories}
#' most frequent values and collapses the rest into an "Other" bucket.
#' Removes the NULL sentinel row and extracts null counts separately.
#'
#' @param cat_hist_df Data frame with columns: table_name, column_name, category_value, freq, null_count
#' @param max_categories Maximum number of category values to display (default 50)
#' @return Data frame with columns: table_name, column_name, category_value, bucket, freq,
#'   nan_count, non_nan_count, hist_type
#' @keywords internal
collapse_to_top_n <- function(cat_hist_df, max_categories = 50) {
  if (nrow(cat_hist_df) == 0) {
    return(data.frame(
      table_name = character(0), column_name = character(0),
      category_value = character(0), bucket = integer(0),
      freq = numeric(0), nan_count = numeric(0), non_nan_count = numeric(0),
      hist_type = character(0), stringsAsFactors = FALSE
    ))
  }

  # Determine grouping columns: include group_key when present
  has_gk <- "group_key" %in% names(cat_hist_df)
  grp_cols <- c("table_name", "column_name")
  if (has_gk) grp_cols <- c(grp_cols, "group_key")

  # Extract null counts (from __NULL__ sentinel rows)
  null_counts <- cat_hist_df %>%
    filter(category_value == "__NULL__") %>%
    group_by(across(all_of(grp_cols))) %>%
    summarise(nan_count = sum(null_count), .groups = "drop")

  # Work with non-null value rows only
  values_df <- cat_hist_df %>%
    filter(category_value != "__NULL__", freq > 0)

  if (nrow(values_df) == 0) {
    return(data.frame(
      table_name = character(0), column_name = character(0),
      category_value = character(0), bucket = integer(0),
      freq = numeric(0), nan_count = numeric(0), non_nan_count = numeric(0),
      hist_type = character(0), stringsAsFactors = FALSE
    ))
  }

  # Rank by frequency and collapse tail into "Other"
  result <- values_df %>%
    group_by(across(all_of(grp_cols))) %>%
    arrange(desc(freq)) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    mutate(
      category_value = ifelse(rank <= max_categories, category_value, "Other"),
      bucket = ifelse(rank <= max_categories, as.integer(rank), max_categories + 1L)
    ) %>%
    group_by(across(all_of(c(grp_cols, "category_value", "bucket")))) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    # Re-rank after collapsing Other
    group_by(across(all_of(grp_cols))) %>%
    arrange(desc(freq)) %>%
    mutate(bucket = row_number()) %>%
    ungroup()

  # Compute non_nan_count per column
  non_nan_totals <- result %>%
    group_by(across(all_of(grp_cols))) %>%
    summarise(non_nan_count = sum(freq), .groups = "drop")

  # Join null counts and totals
  result <- result %>%
    left_join(null_counts, by = grp_cols) %>%
    left_join(non_nan_totals, by = grp_cols) %>%
    mutate(
      nan_count = ifelse(is.na(nan_count), 0, nan_count),
      non_nan_count = ifelse(is.na(non_nan_count), 0, non_nan_count),
      hist_type = "categorical"
    )

  return(result)
}

#' Build Categorical Histogram Scripts for All Tables
#'
#' Prepares categorical histogram queries and metadata for all tables.
#'
#' @param columns_df Data frame of text columns
#' @param runid Run ID to filter data
#' @param schema Output schema for histogram tables (default "dr4_ops_cs48_mv")
#' @param join_clauses Named list of table-specific JOIN clauses
#' @param default_join_clause Default JOIN clause for tables not in join_clauses
#' @param table_info Data frame with system column flags
#' @return Named list of script info, one entry per table
#' @keywords internal
build_categorical_scripts <- function(columns_df, runid,
                                      schema = "dr4_ops_cs48_mv",
                                      join_clauses = NULL,
                                      default_join_clause = NULL,
                                      table_info = NULL,
                                      group_key = NULL) {

  tables <- unique(columns_df$table_name)
  cat(sprintf("Building categorical histogram queries for %d tables...\n", length(tables)))

  scripts <- list()

  for (tbl in tables) {
    tbl_cols <- columns_df[columns_df$table_name == tbl, ]
    if (nrow(tbl_cols) == 0) next

    if (!is.null(join_clauses) && tbl %in% names(join_clauses)) {
      join_clause <- join_clauses[[tbl]]
    } else {
      join_clause <- default_join_clause
    }

    sql_query <- build_categorical_histogram_query(
      table_name = tbl,
      columns_df = columns_df,
      runid = runid,
      join_clause = join_clause,
      table_info = table_info,
      group_key = group_key
    )

    if (is.null(sql_query)) next

    # Check if this table should use parallel execution
    tbl_use_parallel <- TRUE
    if (!is.null(table_info)) {
      ti <- table_info[table_info$table_name == tbl, ]
      if (nrow(ti) > 0) {
        tbl_use_parallel <- if ("use_parallel" %in% names(ti)) isTRUE(ti$use_parallel[1]) else isTRUE(ti$has_sourceid[1])
      }
    }

    output_table <- sprintf("%s.cat_hist_%s_%d", schema, sanitize_identifier(tbl, 45), runid)
    n_cols <- nrow(tbl_cols)

    scripts[[tbl]] <- list(
      sql_query = sql_query,
      source_table = tbl,
      output_table = output_table,
      n_columns = n_cols,
      join_clause = join_clause,
      has_sourceid = tbl_use_parallel,
      aggregation_query = build_categorical_aggregation_query(output_table, group_key = group_key)
    )

    cat(sprintf("  %s: %d text columns -> %s\n", tbl, n_cols, output_table))
  }

  return(scripts)
}

#' Execute Categorical Histogram Scripts
#'
#' Executes prepared categorical histogram scripts via partParalXZ4 and
#' aggregates the partial results.
#'
#' @param scripts Named list of script info (from build_categorical_scripts)
#' @param runid Run ID for partitioning
#' @param db_user Database user for execution
#' @param conn DBI database connection for aggregation
#' @param slack_user Slack user for notifications (default "@nienarto")
#' @param parallelism Number of parallel workers (default 80)
#' @param num_chunks Number of data chunks (default 600)
#' @param execute If TRUE, execute scripts; if FALSE, return scripts only
#' @param debug If TRUE, print detailed debug output
#' @return List with execution results and combined categorical histograms
#' @keywords internal
execute_categorical_scripts <- function(scripts, runid, db_user, conn = NULL,
                                        slack_user = "@nienarto", parallelism = 80,
                                        num_chunks = 600, execute = FALSE, debug = FALSE) {
  results <- list()
  all_histograms <- list()

  for (i in seq_along(scripts)) {
    script_info <- scripts[[i]]
    tbl <- names(scripts)[i]

    if (debug) {
      cat(sprintf("\n=== CATEGORICAL TABLE %d/%d: %s ===\n", i, length(scripts), tbl))
      cat(sprintf("Output table: %s\n", script_info$output_table))
      cat(sprintf("Columns: %d\n", script_info$n_columns))
      cat("\n--- Query (first 2000 chars) ---\n")
      cat(substr(script_info$sql_query, 1, 2000))
      if (nchar(script_info$sql_query) > 2000) cat("\n... [truncated]")
      cat("\n==================\n\n")
    }

    if (execute) {
      tbl_has_sourceid <- isTRUE(script_info$has_sourceid)

      cat(sprintf("Executing categorical %d/%d: %s (%d columns)%s...\n",
                  i, length(scripts), tbl, script_info$n_columns,
                  if (!tbl_has_sourceid) " [direct - no sourceid]" else ""))

      if (tbl_has_sourceid) {
        exit_code <- execute_parallel_script(
          runid = runid,
          output_table = script_info$output_table,
          sql_query = script_info$sql_query,
          db_user = db_user,
          slack_user = slack_user,
          parallelism = parallelism,
          num_chunks = num_chunks,
          description = sprintf("CatHist %s", tbl)
        )

        if (exit_code != 0) {
          warning(sprintf("Categorical script for %s failed with exit code %d", tbl, exit_code))
          results[[tbl]] <- list(success = FALSE, exit_code = exit_code)
        } else {
          results[[tbl]] <- list(
            success = TRUE,
            source_table = tbl,
            output_table = script_info$output_table,
            n_columns = script_info$n_columns
          )

          if (!is.null(conn)) {
            cat(sprintf("  Aggregating categorical results for %s...\n", tbl))
            agg_result <- dbGetQuery(conn, script_info$aggregation_query)
            results[[tbl]]$histogram_data <- agg_result
            all_histograms[[tbl]] <- agg_result
          }
        }
      } else {
        # Direct execution for tables without sourceid
        if (!is.null(conn)) {
          direct_result <- tryCatch({
            dbGetQuery(conn, script_info$sql_query)
          }, error = function(e) {
            warning(sprintf("Direct categorical query for %s failed: %s", tbl, e$message))
            NULL
          })

          if (!is.null(direct_result) && nrow(direct_result) > 0) {
            results[[tbl]] <- list(
              success = TRUE,
              source_table = tbl,
              n_columns = script_info$n_columns,
              histogram_data = direct_result
            )
            all_histograms[[tbl]] <- direct_result
          } else {
            results[[tbl]] <- list(success = FALSE, reason = "direct query returned no data")
          }
        } else {
          warning(sprintf("Table %s has no sourceid and no conn for direct execution", tbl))
          results[[tbl]] <- list(success = FALSE, reason = "no sourceid, no conn")
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
    results$combined_categorical <- bind_rows(all_histograms)
  }

  return(results)
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
  cmd <- sprintf("cat '%s' | partParalXZ4 %d %s %s %s %d false private dpcg-dex '%s' %d",
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
                                    default_join_clause = NULL,
                                    table_info = NULL,
                                    group_key = NULL) {

  tables <- unique(columns_df$table_name)

  cat(sprintf("Building histogram queries for %d tables...\n", length(tables)))

  # Handle empty or malformed global_stats
  required_cols <- c("table_name", "column_name", "global_min", "global_max", "non_nan_count")
  if (nrow(global_stats) == 0 || !all(required_cols %in% names(global_stats))) {
    cat("  No valid global stats available, skipping histogram queries\n")
    return(list())
  }

  # Filter out columns with invalid stats before building queries
  valid_stats <- global_stats %>%
    filter(
      !is.na(global_min) & !is.na(global_max) &
        !is.infinite(global_min) & !is.infinite(global_max) &
        !is.nan(global_min) & !is.nan(global_max) &
        global_min <= global_max &
        non_nan_count > 0
    )

  skipped_cols <- nrow(global_stats) - nrow(valid_stats)
  if (skipped_cols > 0) {
    skipped <- global_stats %>%
      anti_join(valid_stats, by = c("table_name", "column_name"))
    cat(sprintf("  Skipping %d columns with invalid stats (Inf/NaN/no data):\n", skipped_cols))
    for (j in seq_len(nrow(skipped))) {
      s <- skipped[j, ]
      cat(sprintf("    %s.%s: min=%s max=%s valid=%s\n",
                  s$table_name, s$column_name, s$global_min, s$global_max, s$non_nan_count))
    }
  }

  # Filter columns_df to only include columns with valid stats
  valid_columns_df <- columns_df %>%
    semi_join(valid_stats, by = c("table_name", "column_name"))

  # Batch size: limits SQL string length to avoid xargs "argument line too long"
  # inside partParalXZ4. Same limit as Phase 1 stats batching.
  max_hist_columns <- 50

  scripts <- list()

  for (tbl in tables) {
    tbl_cols <- valid_columns_df %>% filter(table_name == tbl)

    if (nrow(tbl_cols) == 0) {
      cat(sprintf("  %s: No valid columns, skipping\n", tbl))
      next
    }

    if (!is.null(join_clauses) && tbl %in% names(join_clauses)) {
      join_clause <- join_clauses[[tbl]]
    } else {
      join_clause <- default_join_clause
    }

    # Check if this table should use parallel execution
    tbl_use_parallel <- TRUE
    if (!is.null(table_info)) {
      ti <- table_info[table_info$table_name == tbl, ]
      if (nrow(ti) > 0) {
        tbl_use_parallel <- if ("use_parallel" %in% names(ti)) isTRUE(ti$use_parallel[1]) else isTRUE(ti$has_sourceid[1])
      }
    }

    n_cols <- nrow(tbl_cols)

    # Split into batches to keep SQL within xargs limits.
    # When group_key is active each column expands to N UNION ALL clauses
    # (one per group value), so the effective size is n_cols * n_groups.
    has_gk <- !is.null(group_key) && nzchar(group_key)
    if (has_gk) {
      tbl_stats <- valid_stats %>% filter(table_name == tbl)
      n_groups <- max(1, length(unique(tbl_stats$group_key)))
      effective_batch_size <- max(1, floor(max_hist_columns / n_groups))
    } else {
      effective_batch_size <- max_hist_columns
    }

    if (n_cols > effective_batch_size) {
      n_batches <- ceiling(n_cols / effective_batch_size)
      batch_indices <- split(seq_len(n_cols), ceiling(seq_len(n_cols) / effective_batch_size))
    } else {
      n_batches <- 1
      batch_indices <- list(seq_len(n_cols))
    }

    if (n_batches > 1) {
      cat(sprintf("  %s: %d columns, splitting into %d batches\n", tbl, n_cols, n_batches))
    }

    for (b in seq_along(batch_indices)) {
      batch_col_names <- tbl_cols$column_name[batch_indices[[b]]]
      batch_columns_df <- valid_columns_df %>%
        filter(table_name == tbl, column_name %in% batch_col_names)
      batch_stats <- valid_stats %>%
        filter(table_name == tbl, column_name %in% batch_col_names)

      sql_query <- build_table_histogram_query(
        table_name = tbl,
        columns_df = batch_columns_df,
        global_stats = batch_stats,
        runid = runid,
        num_buckets = num_buckets,
        join_clause = join_clause,
        table_info = table_info,
        group_key = group_key
      )

      if (is.null(sql_query)) next

      if (n_batches > 1) {
        batch_key <- sprintf("%s__batch%d", tbl, b)
        output_table <- sprintf("%s.hist_%s_b%d_%d", schema,
                                 sanitize_identifier(tbl, 40), b, runid)
        cat(sprintf("  %s [batch %d/%d]: %d columns -> %s\n",
                    tbl, b, n_batches, nrow(batch_columns_df), output_table))
      } else {
        batch_key <- tbl
        output_table <- sprintf("%s.hist_%s_%d", schema, sanitize_identifier(tbl, 50), runid)
        cat(sprintf("  %s: %d valid columns -> %s\n", tbl, n_cols, output_table))
      }

      scripts[[batch_key]] <- list(
        sql_query = sql_query,
        source_table = tbl,
        output_table = output_table,
        n_columns = nrow(batch_columns_df),
        join_clause = join_clause,
        has_sourceid = tbl_use_parallel,
        aggregation_query = build_histogram_aggregation_query(output_table, group_key = group_key)
      )
    }
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
      tbl_has_sourceid <- isTRUE(script_info$has_sourceid)

      cat(sprintf("Executing %d/%d: %s (%d columns)%s...\n",
                  i, length(scripts), tbl, script_info$n_columns,
                  if (!tbl_has_sourceid) " [direct - no sourceid]" else ""))

      if (tbl_has_sourceid) {
        # Parallel execution via partParalXZ4
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
        # Direct execution for tables without sourceid
        if (!is.null(conn)) {
          direct_result <- tryCatch({
            dbGetQuery(conn, script_info$sql_query)
          }, error = function(e) {
            warning(sprintf("Direct query for %s failed: %s", tbl, e$message))
            NULL
          })

          if (!is.null(direct_result) && nrow(direct_result) > 0) {
            results[[tbl]] <- list(
              success = TRUE,
              source_table = tbl,
              n_columns = script_info$n_columns,
              histogram_data = direct_result
            )
            all_histograms[[tbl]] <- direct_result
          } else {
            results[[tbl]] <- list(success = FALSE, reason = "direct query returned no data")
          }
        } else {
          warning(sprintf("Table %s has no sourceid and no conn provided for direct execution", tbl))
          results[[tbl]] <- list(success = FALSE, reason = "no sourceid, no conn")
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
#' using the actual bucket_min/bucket_max values from the SQL query for
#' accurate display. Also computes freq_pct (frequency percentage).
#'
#' @param histogram_df Data frame with histogram data
#' @param num_buckets Number of histogram buckets (must match original query)
#' @return Data frame with additional boundary columns
#' @export
compute_bucket_boundaries <- function(histogram_df, num_buckets = 20) {
  # Add hist_type if not present (backward compatibility)
  if (!"hist_type" %in% names(histogram_df)) {
    histogram_df$hist_type <- "numeric"
  }

  # Split: only apply numeric boundary logic to numeric rows
  numeric_df <- histogram_df %>% filter(hist_type == "numeric")
  categorical_df <- histogram_df %>% filter(hist_type == "categorical")

  # Determine grouping columns: include group_key when present
  has_gk <- "group_key" %in% names(histogram_df)
  grp_cols <- c("table_name", "column_name")
  if (has_gk) grp_cols <- c(grp_cols, "group_key")

  if (nrow(numeric_df) > 0) {
    # Ensure total_count exists (backward compat with older stats)
    if (!"total_count" %in% names(numeric_df)) {
      numeric_df$total_count <- NA_real_
    }
    numeric_df <- numeric_df %>%
      mutate(
        freq = as.numeric(freq),
        nan_count = as.numeric(nan_count),
        inf_count = as.numeric(inf_count),
        non_nan_count = as.numeric(non_nan_count),
        total_count = as.numeric(total_count),
        global_min = as.numeric(global_min),
        global_max = as.numeric(global_max),
        bucket_min = as.numeric(bucket_min),
        bucket_max = as.numeric(bucket_max),
        bucket_avg = as.numeric(bucket_avg)
      ) %>%
      group_by(across(all_of(grp_cols))) %>%
      mutate(
        bucket_lower = bucket_min,
        bucket_upper = bucket_max,
        bucket_center = (bucket_lower + bucket_upper) / 2,
        bucket_width = bucket_upper - bucket_lower,
        freq_pct = freq / sum(freq) * 100
      ) %>%
      ungroup()
  }

  if (nrow(categorical_df) > 0) {
    categorical_df <- categorical_df %>%
      mutate(freq = as.numeric(freq)) %>%
      group_by(across(all_of(grp_cols))) %>%
      mutate(freq_pct = freq / sum(freq) * 100) %>%
      ungroup()
  }

  bind_rows(numeric_df, categorical_df)
}

#' Run Histogram Analysis
#'
#' Main workflow function for generating histograms of numeric and categorical
#' columns in PostgreSQL tables using parallel execution. The analysis is
#' performed in three phases:
#'
#' 1. **Phase 1**: Compute global min/max statistics for all numeric columns
#'    using parallel execution via partParalXZ4, then aggregate results
#' 2. **Phase 2**: Execute parallel bucketing queries using fixed global
#'    boundaries via partParalXZ4
#' 3. **Phase 3**: Compute categorical (text) column frequency distributions
#'    via parallel GROUP BY, then collapse to top N values
#'
#' The two-phase approach for numeric columns ensures consistent bucket
#' boundaries across all parallel chunks. Categorical columns use a simpler
#' single-phase approach with post-aggregation collapsing.
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
#' @param array_elements Number of array elements to expand per array column (default 10).
#'   For each array column, histograms are generated for elements 1 through \code{array_elements}
#'   plus the array length. Elements beyond the actual array size are automatically filtered out.
#' @param max_categories Maximum number of category values to show per text column (default 50).
#'   Values beyond the top N are collapsed into an "Other" bucket.
#' @param text_columns_fn Function to discover text columns (default get_histogram_text_columns).
#'   Set to NULL to skip categorical histograms entirely.
#' @param slack_user Slack user for notifications (default "@nienarto")
#' @param parallelism Number of parallel workers for partParalXZ4 (default 80)
#' @param num_chunks Number of data chunks for partParalXZ4 (default 600)
#' @param min_parallel_rows Minimum estimated row count (across all nodes) for
#'   a table to use parallel execution via partParalXZ4. Tables below this
#'   threshold run as direct single queries. Default 10 million. Row counts
#'   are estimated from pg_class.reltuples multiplied by 6 (data nodes).
#' @param execute If TRUE, execute scripts; if FALSE, return scripts only (dry run)
#' @param debug If TRUE, print detailed debug output
#' @param group_key Optional SQL expression for per-group histograms (e.g.,
#'   \code{"tag::text"}). When non-NULL, produces independent histograms per
#'   group value with separate min/max and bucket boundaries. Default NULL
#'   (no grouping, identical to original behavior).
#' @return List containing:
#'   \itemize{
#'     \item Per-table results with success status and histogram data
#'     \item combined_histograms: All numeric histograms in one data frame
#'     \item combined_categorical: All categorical histograms in one data frame
#'     \item histogram_for_viz: Combined numeric + categorical with computed boundaries
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
                                   columns_fn = get_histogram_columns,
                                   array_elements = 10,
                                   max_categories = 50,
                                   text_columns_fn = get_histogram_text_columns,
                                   slack_user = "@nienarto",
                                   parallelism = 80,
                                   num_chunks = 600,
                                   min_parallel_rows = 10e6,
                                   execute = FALSE,
                                   debug = FALSE,
                                   group_key = NULL) {

  conn <- dpcgR::connect(hostname = inparams$hostname, port = inparams$dbPort, user = inparams$dbUser)

  cat(sprintf("=== HISTOGRAM ANALYSIS FOR MODULE '%s', RUNID %d ===\n\n", module, runid))
  cat(sprintf("Using database user: %s\n", inparams$dbUser))
  has_group_key <- !is.null(group_key) && nzchar(group_key)
  if (has_group_key) {
    cat(sprintf("Group key: %s (per-group independent histograms)\n", group_key))
  }
  cat("\n")

  # Auto-select matching text columns function if using mdb numeric variant
  if (identical(text_columns_fn, get_histogram_text_columns) &&
      identical(columns_fn, get_histogram_mdb_columns)) {
    text_columns_fn <- get_histogram_mdb_text_columns
    cat("Auto-selected get_histogram_mdb_text_columns for MDB mode\n\n")
  }

  # Get column metadata and system column info
  # Pass array_elements if columns_fn accepts it (our built-in functions do)
  col_result <- tryCatch(
    columns_fn(conn, module, array_elements = array_elements),
    error = function(e) {
      # Fallback for custom columns_fn that don't accept array_elements
      columns_fn(conn, module)
    }
  )
  if (is.data.frame(col_result)) {
    # Backwards compatibility: if columns_fn returns a plain data frame
    columns_df <- col_result
    table_info <- NULL
  } else {
    columns_df <- col_result$columns
    table_info <- col_result$table_info
  }
  cat(sprintf("Found %d columns across %d tables\n\n",
              nrow(columns_df), length(unique(columns_df$table_name))))

  # Estimate table sizes and decide parallel vs direct execution per table
  all_tables <- unique(columns_df$table_name)
  row_estimates <- estimate_table_rows(conn, all_tables)

  if (!is.null(table_info)) {
    # Add use_parallel flag: needs sourceid AND known row count above threshold
    # When reltuples is 0 or unknown, default to direct (non-parallel) execution
    table_info$use_parallel <- sapply(table_info$table_name, function(tbl) {
      has_sid <- table_info$has_sourceid[table_info$table_name == tbl]
      est <- row_estimates[tbl]
      isTRUE(has_sid) && !is.na(est) && est > 0 && est >= min_parallel_rows
    })

    cat("Table execution plan:\n")
    for (r in seq_len(nrow(table_info))) {
      ti <- table_info[r, ]
      est <- row_estimates[ti$table_name]
      cat(sprintf("  %s: ~%s rows, sourceid=%s -> %s\n",
                  ti$table_name,
                  format(est, big.mark = ",", scientific = FALSE),
                  ti$has_sourceid,
                  if (isTRUE(ti$use_parallel)) "PARALLEL" else "DIRECT"))
    }
    cat("\n")
  }

  # PHASE 1: Compute global statistics (parallel execution)
  cat("=== PHASE 1: Computing global statistics (parallel execution) ===\n")
  global_stats <- compute_global_stats(
    conn = conn,
    columns_df = columns_df,
    runid = runid,
    join_clauses = join_clauses,
    default_join_clause = default_join_clause,
    db_user = inparams$dbUser,
    schema = schema,
    slack_user = slack_user,
    parallelism = parallelism,
    num_chunks = num_chunks,
    execute = execute,
    debug = debug,
    table_info = table_info,
    group_key = group_key
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
    default_join_clause = default_join_clause,
    table_info = table_info,
    group_key = group_key
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
      numeric_viz <- results$combined_histograms %>%
        mutate(across(where(bit64::is.integer64), as.numeric))
      numeric_viz$hist_type <- "numeric"
      numeric_viz$category_value <- NA_character_
    } else {
      numeric_viz <- NULL
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
    numeric_viz <- NULL
  }

  # PHASE 3: Categorical (text) column histograms
  cat_columns_df <- NULL
  cat_table_info <- NULL
  if (!is.null(text_columns_fn)) {
    cat("\n=== PHASE 3: Categorical column histograms ===\n")
    cat_col_result <- tryCatch(
      text_columns_fn(conn, module),
      error = function(e) {
        if (debug) cat(sprintf("  Text column discovery failed: %s\n", e$message))
        list(columns = data.frame(), table_info = NULL)
      }
    )

    if (is.data.frame(cat_col_result)) {
      cat_columns_df <- cat_col_result
    } else {
      cat_columns_df <- cat_col_result$columns
      cat_table_info <- cat_col_result$table_info
    }

    if (!is.null(cat_columns_df) && nrow(cat_columns_df) > 0) {
      cat(sprintf("Found %d text columns across %d tables\n\n",
                  nrow(cat_columns_df), length(unique(cat_columns_df$table_name))))

      cat_scripts <- build_categorical_scripts(
        columns_df = cat_columns_df,
        runid = runid,
        schema = schema,
        join_clauses = join_clauses,
        default_join_clause = default_join_clause,
        table_info = if (!is.null(cat_table_info)) cat_table_info else table_info,
        group_key = group_key
      )
      cat("\n")

      if (execute) {
        cat("Executing categorical histogram scripts...\n")
        cat_results <- execute_categorical_scripts(
          scripts = cat_scripts,
          runid = runid,
          db_user = inparams$dbUser,
          conn = conn,
          slack_user = slack_user,
          parallelism = parallelism,
          num_chunks = num_chunks,
          execute = TRUE,
          debug = debug
        )

        if (!is.null(cat_results$combined_categorical) &&
            nrow(cat_results$combined_categorical) > 0) {
          cat_combined <- cat_results$combined_categorical %>%
            mutate(across(where(bit64::is.integer64), as.numeric))
          cat_viz <- collapse_to_top_n(cat_combined, max_categories)
          results$combined_categorical <- cat_results$combined_categorical
        } else {
          cat_viz <- NULL
        }
      } else {
        cat_results <- execute_categorical_scripts(
          scripts = cat_scripts,
          runid = runid,
          db_user = inparams$dbUser,
          conn = NULL,
          execute = FALSE,
          debug = debug
        )
        cat_viz <- NULL
      }

      # Store categorical script results
      results$categorical_scripts <- cat_results
    } else {
      cat("No text columns found\n")
      cat_viz <- NULL
    }
  } else {
    cat_viz <- NULL
  }

  # Merge numeric and categorical into histogram_for_viz
  viz_parts <- list()
  if (!is.null(numeric_viz)) {
    viz_parts <- c(viz_parts, list(numeric_viz))
  }
  if (!is.null(cat_viz) && nrow(cat_viz) > 0) {
    viz_parts <- c(viz_parts, list(cat_viz))
  }
  if (length(viz_parts) > 0) {
    results$histogram_for_viz <- compute_bucket_boundaries(
      bind_rows(viz_parts), num_buckets
    )
  }

  dbDisconnect(conn)

  cat("\n=== HISTOGRAM ANALYSIS COMPLETE ===\n")

  results$metadata <- list(
    runid = runid,
    module = module,
    num_buckets = num_buckets,
    max_categories = max_categories,
    n_tables = length(scripts),
    n_columns = nrow(columns_df),
    n_text_columns = if (!is.null(cat_columns_df)) nrow(cat_columns_df) else 0,
    tables = names(scripts),
    global_stats = global_stats,
    db_user = inparams$dbUser,
    group_key = group_key
  )

  return(results)
}
