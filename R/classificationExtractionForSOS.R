# =============================================================================
# Classifier Analysis Pipeline for Astronomical Source Classification
# =============================================================================
#
# This module provides functions for extracting classifier configurations from
# a PostgreSQL database, building optimized SQL queries for skymap aggregation,
# and executing parallel analysis workflows for astronomical source classification
# results from the Gaia mission.
#
# @keywords internal
# =============================================================================

# Load required libraries
#' @import DBI
#' @import parallel
#' @import tidyr
#' @import dplyr
#' @importFrom jsonlite fromJSON
NULL


# =============================================================================
# CONFIGURATION EXTRACTION
# =============================================================================

#' Extract Classifier Configuration from Database
#'
#' Retrieves classifier configuration from the database for a specific run and
#' SOS (Specific Object Study) configuration. Parses XML configuration to extract
#' classifier definitions, probability thresholds, and type mappings.
#'
#' @param conn A DBI database connection object.
#' @param runid Integer. The run identifier to query configuration for.
#' @param sosname_conf Character string. The SOS algorithm configuration identifier
#'   (e.g., "sos_ceprrl_conf").
#'
#' @return A dataframe with columns:
#'   \describe{
#'     \item{sosname}{Character. The SOS name extracted from configuration}
#'     \item{all_types}{Character. Comma-separated list of all classifier:type combinations}
#'     \item{cuts_per_classifier}{Character. JSON object containing probability thresholds
#'       per classifier and type}
#'   }
#'
#' @details
#' The function uses recursive CTEs to parse nested XML configuration structure:
#' \enumerate{
#'   \item Extracts SOS-level configuration from run's fconfiguration
#'   \item Parses classifier definitions within selection conditions
#'   \item Extracts group definitions with probability thresholds
#'   \item Aggregates type conditions and cut thresholds per classifier
#' }
#'
#' @section Database Requirements:
#' Requires access to the \code{run} table with \code{fconfiguration} XML column.
#' Uses PostgreSQL-specific functions: \code{xpath()}, \code{unnest()}, \code{jsonb_object_agg()}.
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(RPostgres::Postgres(), dbname = "gaia")
#' config <- extract_classifier_config(conn, runid = 90005, sosname_conf = "sos_ceprrl_conf
#' ")
#' print(config)
#' DBI::dbDisconnect(conn)
#' }
#'
#' @seealso \code{\link{parse_type_conditions}} for parsing the returned configuration
#'
#' @export
extract_classifier_config <- function(conn, runid, sosname_conf) {

  config_query <- sprintf("
    WITH RECURSIVE hierarchy AS (
        SELECT
            (xpath('local-name(/*)', x))[1]::text AS sosname,
            x AS sos_xml,
            1 AS level,
            (xpath('local-name(/*)', x))[1]::text AS path
        FROM run p_run,
        LATERAL unnest(xpath('/gaiaConfig/cu7/algo/sos/*', p_run.fconfiguration)) x
        WHERE
        runid = %s AND (xpath('//@algoConfigId', x))[1]::text = '%s'
    ),
    classifiers AS (
        SELECT
            h.sosname,
            h.sos_xml,
            (xpath('//classifierDefinitionName/text()', c))[1]::text AS classifier_name,
            c AS classifier_xml
        FROM hierarchy h,
        LATERAL unnest(xpath('//selectionConditions//classifierDefinitions/classifier', h.sos_xml)) c
    ),
    groups AS (
        SELECT
            c.sosname,
            c.classifier_name,
            (xpath('/group/@groupName', g))[1]::text AS group_name,
            (xpath('/group/@probabilityThreshold', g))[1]::text AS threshold,
            g AS group_xml
        FROM classifiers c,
        LATERAL unnest(xpath('//variabilityGroupTypesToSelect/group', c.classifier_xml)) g
    ),
    types AS (
        SELECT
            g.sosname,
            g.classifier_name,
            g.group_name,
            g.threshold,
            (xpath('/type/text()', t))[1]::text AS type_value
        FROM groups g,
        LATERAL unnest(xpath('/group/type', g.group_xml)) t
    ),
    type_conditions AS (
        SELECT
            sosname,
            classifier_name,
            group_name,
            threshold,
            type_value,
            classifier_name || ':' || type_value AS prefixed_type
        FROM types
    ),
    cuts_aggregated AS (
        SELECT
            sosname,
            jsonb_object_agg(
                classifier_name,
                (SELECT jsonb_object_agg(prefixed_type, cut_threshold)
                 FROM type_conditions tc3
                 WHERE tc3.sosname = tc.sosname AND tc3.classifier_name = tc.classifier_name)
            ) AS cuts_per_classifier
        FROM (
            SELECT DISTINCT sosname, classifier_name, prefixed_type, threshold::float AS cut_threshold
            FROM type_conditions
        ) tc
        GROUP BY sosname
    ),
    classifier_aggregates AS (
        SELECT
            sosname,
            classifier_name,
            array_agg(prefixed_type ORDER BY prefixed_type) AS all_types
        FROM type_conditions
        GROUP BY sosname, classifier_name
    ),
    algo_aggregates AS (
        SELECT
            ca.sosname,
            (SELECT array_agg(DISTINCT elem ORDER BY elem)
             FROM (SELECT unnest(all_types) AS elem FROM classifier_aggregates ca2 WHERE ca2.sosname = ca.sosname) sub
            ) AS all_types
        FROM classifier_aggregates ca
        GROUP BY ca.sosname
    )
    SELECT
        aa.sosname,
        aa.all_types,
        ca.cuts_per_classifier
    FROM algo_aggregates aa
    JOIN cuts_aggregated ca ON aa.sosname = ca.sosname
    ORDER BY aa.sosname;
  ", runid, sosname_conf)

  result <- dbGetQuery(conn, config_query)
  return(result)
}


#' Parse Type Conditions from Configuration Row
#'
#' Converts a raw configuration row from \code{\link{extract_classifier_config}}
#' into a structured list containing type conditions, SQL filter expressions,
#' and probability cut thresholds.
#'
#' @param config_row A single-row dataframe from \code{extract_classifier_config}
#'   containing columns: \code{sosname}, \code{all_types}, \code{cuts_per_classifier}.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{sosname}{Character. The SOS name}
#'     \item{types}{Character vector. All classifier:type combinations}
#'     \item{type_conditions}{Named list. SQL WHERE clause fragments for each type,
#'       incorporating classifier ID and probability threshold}
#'     \item{cuts}{Named nested list. Probability thresholds organized as
#'       \code{cuts[[classifier]][[type]]}}
#'     \item{type_to_classname}{Named list. Mapping from type value to full
#'       classifier:type identifier}
#'   }
#'
#' @details
#' The function generates SQL conditions in the format:
#' \preformatted{
#' (classifierid = getclassifierid('classifier_name')
#'  AND (posteriorprobabilities->'type_val') IS NOT NULL
#'  AND (posteriorprobabilities->'type_val')::float > threshold)
#' }
#'
#' @examples
#' \dontrun{
#' config <- extract_classifier_config(conn, 90005, "sos_ceprrl_conf")
#' type_conditions <- parse_type_conditions(config[1, ])
#' names(type_conditions$cuts)
#' # [1] "CD_DR4_xgboost_multiclass_251120" "CD_agn_rrl_eb_cep_source_selection_251211_1"
#' }
#'
#' @seealso \code{\link{extract_classifier_config}} for obtaining configuration data
#'
#' @importFrom jsonlite fromJSON
#' @export
parse_type_conditions <- function(config_row) {
  # Parse array string to vector
  all_types <- gsub("^\\{|\\}$", "", config_row$all_types)
  all_types <- strsplit(all_types, ",")[[1]]

  # Parse JSON cuts
  cuts <- jsonlite::fromJSON(config_row$cuts_per_classifier)

  type_conditions <- list()
  type_to_classname <- list()

  for (type in all_types) {
    parts <- strsplit(type, ":")[[1]]
    classifier <- parts[1]
    type_val <- parts[2]

    type_to_classname[[type_val]] <- type

    if (!is.null(cuts[[classifier]][[type]])) {
      threshold <- cuts[[classifier]][[type]]
      type_conditions[[type]] <- sprintf(
        "(classifierid = getclassifierid('%s') AND (posteriorprobabilities->'%s') IS NOT NULL AND (posteriorprobabilities->'%s')::float > %s)",
        classifier, type_val, type_val, threshold
      )
    }
  }

  list(
    sosname = config_row$sosname,
    types = all_types,
    type_conditions = type_conditions,
    cuts = cuts,
    type_to_classname = type_to_classname
  )
}


# =============================================================================
# HELPER FUNCTIONS FOR IDENTIFIER MANAGEMENT
# =============================================================================

#' Sanitize Identifier for PostgreSQL
#'
#' Converts a string into a valid PostgreSQL identifier by replacing invalid
#' characters, removing consecutive underscores, and truncating to maximum length.
#'
#' @param name Character string. The identifier to sanitize.
#' @param max_length Integer. Maximum allowed length for the identifier.
#'   Default is 63 (PostgreSQL's maximum identifier length).
#'
#' @return A sanitized character string safe for use as a PostgreSQL identifier.
#'
#' @details
#' Transformations applied:
#' \enumerate{
#'   \item Replace dashes with underscores
#'   \item Replace non-alphanumeric characters (except underscore) with underscores
#'   \item Collapse consecutive underscores
#'   \item Remove leading/trailing underscores
#'   \item Truncate to \code{max_length} characters
#'   \item Remove trailing underscore after truncation
#' }
#'
#' @examples
#' sanitize_identifier("CD_DR4-xgboost-multiclass_250912_v0")
#' # Returns: "CD_DR4_xgboost_multiclass_250912_v0"
#'
#' sanitize_identifier("very-long-classifier-name-that-exceeds-limit", max_length = 20)
#' # Returns: "very_long_classifier" (truncated)
#'
#' @keywords internal
#' @export
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


#' Create Short Column Alias
#'
#' Generates a short, unique column alias for use in SQL queries to avoid
#' exceeding PostgreSQL's identifier length limits.
#'
#' @param type_index Integer. The index of the type in the types list (1-based).
#'
#' @return A character string in format "c{index}" (e.g., "c1", "c2", "c15").
#'
#' @details
#' Column aliases need to accommodate suffixes like "_training_count_with_cut"
#' (24 characters). Using short numeric indices (c1, c2, etc.) ensures the
#' full column name stays well under PostgreSQL's 63-character limit.
#'
#' Suffix examples and their lengths:
#' \itemize{
#'   \item \code{_cnt_nc} (7 chars) - count, no cut
#'   \item \code{_avg_wc} (7 chars) - average, with cut
#'   \item \code{_tr_nc} (6 chars) - training count, no cut
#' }
#'
#' @examples
#' create_column_alias(1)   # Returns: "c1"
#' create_column_alias(15)  # Returns: "c15"
#'
#' @keywords internal
#' @export
create_column_alias <- function(type_index) {
  sprintf("c%d", type_index)
}


#' Build Alias Map for Types
#'
#' Creates a mapping from full type identifiers to short column aliases
#' for use in SQL query generation.
#'
#' @param all_types Character vector. Full type identifiers in format
#'   "classifier_name:type_value".
#'
#' @return A named list where names are full type identifiers and values
#'   are short aliases (c1, c2, etc.).
#'
#' @examples
#' types <- c("CD_DR4_xgboost:CEP", "CD_DR4_xgboost:RR", "CD_source_selection:cepIn")
#' alias_map <- build_alias_map(types)
#' # Returns: list("CD_DR4_xgboost:CEP" = "c1",
#' #               "CD_DR4_xgboost:RR" = "c2",
#' #               "CD_source_selection:cepIn" = "c3")
#'
#' @seealso \code{\link{create_column_alias}}, \code{\link{build_alias_legend}}
#'
#' @export
build_alias_map <- function(all_types) {
  alias_map <- list()
  for (i in seq_along(all_types)) {
    alias_map[[all_types[i]]] <- create_column_alias(i)
  }
  return(alias_map)
}


#' Build Unique Table Name
#'
#' Constructs a fully-qualified PostgreSQL table name from components,
#' ensuring the result is within identifier length limits.
#'
#' @param schema Character string. The database schema name.
#' @param prefix Character string. Table name prefix (e.g., "skymap", "histogram").
#' @param classifier_name Character string. The classifier identifier.
#' @param class_name Character string or NULL. Optional class/type name to include.
#' @param suffix Character string or NULL. Optional suffix (e.g., "merged", "partial").
#'
#' @return A character string in format "schema.table_name" where table_name
#'   is sanitized and within PostgreSQL limits.
#'
#' @details
#' Components are joined with underscores after sanitization. The final
#' table name (excluding schema) is truncated to 63 characters if necessary.
#'
#' @examples
#' build_table_name("dr4_ops", "skymap", "cepheidandrrlyrae", suffix = "merged")
#' # Returns: "dr4_ops.skymap_cepheidandrrlyrae_merged"
#'
#' build_table_name("dr4_ops", "hist", "CD_DR4_xgboost", "CEP", "with_cut")
#' # Returns: "dr4_ops.hist_CD_DR4_xgboost_CEP_with_cut"
#'
#' @seealso \code{\link{sanitize_identifier}}
#'
#' @export
build_table_name <- function(schema, prefix, classifier_name, class_name = NULL, suffix = NULL) {
  prefix_clean <- sanitize_identifier(prefix)
  classifier_clean <- sanitize_identifier(classifier_name)

  if (!is.null(class_name)) {
    class_clean <- sanitize_identifier(class_name)
  }
  if (!is.null(suffix)) {
    suffix_clean <- sanitize_identifier(suffix)
  }

  if (!is.null(class_name) && !is.null(suffix)) {
    base_name <- paste(prefix_clean, classifier_clean, class_clean, suffix_clean, sep = "_")
  } else if (!is.null(class_name)) {
    base_name <- paste(prefix_clean, classifier_clean, class_clean, sep = "_")
  } else if (!is.null(suffix)) {
    base_name <- paste(prefix_clean, classifier_clean, suffix_clean, sep = "_")
  } else {
    base_name <- paste(prefix_clean, classifier_clean, sep = "_")
  }

  base_name <- sanitize_identifier(base_name, max_length = 63)

  return(sprintf("%s.%s", schema, base_name))
}


#' Build Classifier ID IN Clause
#'
#' Generates a SQL IN clause for filtering by multiple classifier IDs
#' using the \code{getclassifierid()} database function.
#'
#' @param classifiers Character vector. Classifier names to include in the clause.
#'
#' @return A character string containing the SQL IN clause, e.g.,
#'   \code{"classifierid IN (getclassifierid('clf1'), getclassifierid('clf2'))"}.
#'
#' @examples
#' classifiers <- c("CD_DR4_xgboost_multiclass", "CD_source_selection")
#' build_classifierid_in_clause(classifiers)
#' # Returns: "classifierid IN (getclassifierid('CD_DR4_xgboost_multiclass'),
#' #                            getclassifierid('CD_source_selection'))"
#'
#' @keywords internal
#' @export
build_classifierid_in_clause <- function(classifiers) {
  getclassifierid_calls <- sapply(classifiers, function(c) sprintf("getclassifierid('%s')", c))
  sprintf("classifierid IN (%s)", paste(getclassifierid_calls, collapse = ", "))
}


# =============================================================================
# SQL QUERY BUILDERS
# =============================================================================

#' Build Merged Skymap Query
#'
#' Generates an optimized SQL query for computing skymap aggregations across
#' all classifier types simultaneously using PostgreSQL FILTER aggregates.
#' The query aggregates classification results by HEALPix coordinates.
#'
#' @param runid Integer. The run identifier to query.
#' @param type_conditions_list List. Output from \code{\link{parse_type_conditions}}
#'   containing types, cuts, and other configuration.
#' @param schema Character string. Database schema containing the training set
#'   table. Default is "dr4_ops_cs48_mv".
#'
#' @return A list with components:
#'   \describe{
#'     \item{query}{Character. The complete SQL query string}
#'     \item{alias_map}{Named list. Mapping from full type identifiers to
#'       column aliases}
#'   }
#'
#' @details
#' The generated query performs the following:
#' \enumerate{
#'   \item Joins classification results with training set membership
#'   \item Converts source IDs to HEALPix level-8 coordinates
#'   \item Computes per-type aggregates using FILTER clauses:
#'     \itemize{
#'       \item Count, average, min, max probability (with and without cuts)
#'       \item Training source counts (with and without cuts)
#'     }
#'   \item Computes merged aggregates across all types
#' }
#'
#' Column naming convention uses short aliases:
#' \itemize{
#'   \item \code{c1_cnt_nc}: Type 1 count, no cut
#'   \item \code{c1_avg_wc}: Type 1 average probability, with cut
#'   \item \code{c1_tr_nc}: Type 1 training count, no cut
#'   \item \code{all_cnt_wc}: Merged count, with cut
#' }
#'
#' @section Performance Notes:
#' Using FILTER aggregates in a single pass is significantly more efficient
#' than multiple separate queries or subqueries. The query is designed for
#' parallel execution using HEALPix-based partitioning.
#'
#' @examples
#' \dontrun{
#' type_conditions <- parse_type_conditions(config[1, ])
#' result <- build_merged_skymap_query(90005, type_conditions)
#' cat(result$query)
#' print(result$alias_map)
#' }
#'
#' @seealso
#' \code{\link{parse_type_conditions}} for input preparation,
#' \code{\link{pivot_skymap_results}} for processing query output
#'
#' @export
build_merged_skymap_query <- function(runid, type_conditions_list, schema = "dr4_ops_cs48_mv") {
  all_types <- type_conditions_list$types

  # Create column alias mapping
  alias_map <- build_alias_map(all_types)

  # Extract unique classifiers
  classifiers <- unique(sapply(all_types, function(type) strsplit(type, ":")[[1]][1]))
  where_clause <- build_classifierid_in_clause(classifiers)

  # Build base conditions (probability not null)
  all_base_conditions <- sapply(all_types, function(type) {
    parts <- strsplit(type, ":")[[1]]
    classifier <- parts[1]
    type_val <- parts[2]
    sprintf("(classifierid = getclassifierid('%s') AND (posteriorprobabilities->'%s') IS NOT NULL)",
            classifier, type_val)
  })
  merged_base_condition <- paste(all_base_conditions, collapse = " OR ")

  # Build cut conditions (probability > threshold)
  all_cut_conditions <- sapply(all_types, function(type) {
    parts <- strsplit(type, ":")[[1]]
    classifier <- parts[1]
    type_val <- parts[2]
    threshold <- type_conditions_list$cuts[[classifier]][[type]]
    sprintf("(classifierid = getclassifierid('%s') AND (posteriorprobabilities->'%s') IS NOT NULL AND (posteriorprobabilities->'%s')::float > %s)",
            classifier, type_val, type_val, threshold)
  })
  merged_cut_condition <- paste(all_cut_conditions, collapse = " OR ")

  # Build FILTER aggregates for each type
  filter_aggregates <- lapply(all_types, function(type) {
    parts <- strsplit(type, ":")[[1]]
    classifier <- parts[1]
    type_val <- parts[2]
    alias <- alias_map[[type]]
    threshold <- type_conditions_list$cuts[[classifier]][[type]]

    base_condition <- sprintf("classifierid = getclassifierid('%s') AND (posteriorprobabilities->'%s') IS NOT NULL",
                              classifier, type_val)
    cut_condition <- sprintf("classifierid = getclassifierid('%s') AND (posteriorprobabilities->'%s') IS NOT NULL AND (posteriorprobabilities->'%s')::float > %s",
                             classifier, type_val, type_val, threshold)

    training_condition_no_cut <- sprintf("%s AND classname = '%s'", base_condition, type_val)
    training_condition_with_cut <- sprintf("%s AND classname = '%s'", cut_condition, type_val)

    # Aggregates without cut
    agg_no_cut <- sprintf("
      COUNT(*) FILTER (WHERE %s) AS %s_cnt_nc,
      AVG((posteriorprobabilities->'%s')::float4) FILTER (WHERE %s) AS %s_avg_nc,
      MIN((posteriorprobabilities->'%s')::float4) FILTER (WHERE %s) AS %s_min_nc,
      MAX((posteriorprobabilities->'%s')::float4) FILTER (WHERE %s) AS %s_max_nc,
      COUNT(*) FILTER (WHERE %s) AS %s_tr_nc",
                          base_condition, alias, type_val, base_condition, alias,
                          type_val, base_condition, alias, type_val, base_condition, alias,
                          training_condition_no_cut, alias)

    # Aggregates with cut
    agg_with_cut <- sprintf("
      COUNT(*) FILTER (WHERE %s) AS %s_cnt_wc,
      AVG((posteriorprobabilities->'%s')::float4) FILTER (WHERE %s) AS %s_avg_wc,
      MIN((posteriorprobabilities->'%s')::float4) FILTER (WHERE %s) AS %s_min_wc,
      MAX((posteriorprobabilities->'%s')::float4) FILTER (WHERE %s) AS %s_max_wc,
      COUNT(*) FILTER (WHERE %s) AS %s_tr_wc",
                            cut_condition, alias, type_val, cut_condition, alias,
                            type_val, cut_condition, alias, type_val, cut_condition, alias,
                            training_condition_with_cut, alias)

    paste(agg_no_cut, agg_with_cut, sep = ",")
  })

  # Build merged aggregates
  all_type_values <- sapply(all_types, function(t) strsplit(t, ":")[[1]][2])
  training_merged_no_cut <- sprintf("(%s) AND classname IN (%s)",
                                    merged_base_condition,
                                    paste0("'", paste(all_type_values, collapse = "','"), "'"))
  training_merged_with_cut <- sprintf("(%s) AND classname IN (%s)",
                                      merged_cut_condition,
                                      paste0("'", paste(all_type_values, collapse = "','"), "'"))

  merged_aggregates <- sprintf("
      COUNT(*) FILTER (WHERE %s) AS all_cnt_nc,
      COUNT(*) FILTER (WHERE %s) AS all_cnt_wc,
      COUNT(*) FILTER (WHERE %s) AS all_tr_nc,
      COUNT(*) FILTER (WHERE %s) AS all_tr_wc",
                               merged_base_condition, merged_cut_condition,
                               training_merged_no_cut, training_merged_with_cut)

  # Assemble final query
  query <- sprintf("
WITH base AS (
  SELECT
    converHealPixtoPositionLevel(((scr.sourceid::bit(64) >> 35)::bit(64)>>(2*(12-8)))::bigint::int, 8) AS coord,
    scr.classifierid,
    scr.posteriorprobabilities,
    ns.classname
  FROM supervisedclassificationresult scr
  LEFT OUTER JOIN %s.training_set_all_class_nsource ns USING (sourceid)
  WHERE scr.runid = %s
    AND catalogid = 'GAIA_DR4_ALL'::cat
    AND %s
    AND sourceid = sourceid
)
SELECT
  coord,
  degrees(coord[1]) AS alpha,
  degrees(coord[2]) AS delta,
  %s,
  %s
FROM base
GROUP BY coord
ORDER BY coord",
  schema, runid, where_clause,
  paste(unlist(filter_aggregates), collapse = ","),
  merged_aggregates)

list(query = query, alias_map = alias_map)
}


# =============================================================================
# RESULT PROCESSING
# =============================================================================

#' Pivot Skymap Results to Long Format
#'
#' Transforms wide-format skymap aggregation results into a long-format dataframe
#' suitable for plotting and analysis. Maps column aliases back to full type
#' identifiers.
#'
#' @param df Dataframe. Raw query results from \code{\link{build_merged_skymap_query}}
#'   containing columns with alias prefixes (c1_cnt_nc, c2_avg_wc, etc.).
#' @param type_list Character vector. Full type identifiers to process.
#' @param alias_map Named list. Mapping from type identifiers to column aliases,
#'   as returned by \code{\link{build_alias_map}}.
#'
#' @return A dataframe in long format with columns:
#'   \describe{
#'     \item{label}{Character. Full type identifier (classifier:type) or "all_conditions"}
#'     \item{cut_type}{Character. Either "no_cut" or "with_cut"}
#'     \item{coord}{Character. HEALPix coordinate}
#'     \item{alpha}{Numeric. Right ascension in degrees}
#'     \item{delta}{Numeric. Declination in degrees}
#'     \item{count}{Numeric. Number of sources in pixel}
#'     \item{avg_prob}{Numeric. Average posterior probability (NA for merged)}
#'     \item{min_prob}{Numeric. Minimum posterior probability (NA for merged)}
#'     \item{max_prob}{Numeric. Maximum posterior probability (NA for merged)}
#'     \item{training_count}{Numeric. Number of training sources in pixel}
#'   }
#'
#' @details
#' The function processes each type in \code{type_list} and creates two rows
#' per HEALPix pixel (one for "no_cut", one for "with_cut"). Rows with zero
#' counts are filtered out to reduce data size.
#'
#' Special handling for "all_conditions" aggregates which only have count
#' and training_count columns (no probability statistics).
#'
#' @examples
#' \dontrun{
#' # After executing skymap query
#' raw_results <- dbGetQuery(conn, query)
#' pivoted <- pivot_skymap_results(raw_results, type_conditions$types, alias_map)
#' unique(pivoted$label)
#' table(pivoted$cut_type)
#' }
#'
#' @seealso
#' \code{\link{build_merged_skymap_query}} for query generation,
#' \code{\link{expand_aliases_in_pivoted}} for expanding aliases after pivoting
#'
#' @importFrom dplyr select mutate filter bind_rows
#' @importFrom rlang sym
#' @export
pivot_skymap_results <- function(df, type_list, alias_map) {
  results <- list()

  for (type in type_list) {
    alias <- alias_map[[type]]

    # Process no_cut variant
    if (paste0(alias, "_cnt_nc") %in% names(df)) {
      type_df_no_cut <- df %>%
        dplyr::select(coord, alpha, delta,
                      count = !!rlang::sym(paste0(alias, "_cnt_nc")),
                      avg_prob = !!rlang::sym(paste0(alias, "_avg_nc")),
                      min_prob = !!rlang::sym(paste0(alias, "_min_nc")),
                      max_prob = !!rlang::sym(paste0(alias, "_max_nc")),
                      training_count = !!rlang::sym(paste0(alias, "_tr_nc"))) %>%
        dplyr::mutate(label = type, cut_type = "no_cut") %>%
        dplyr::filter(count > 0) %>%
        dplyr::select(label, cut_type, coord, alpha, delta, count, avg_prob, min_prob, max_prob, training_count)

      results[[paste0(type, "_no_cut")]] <- type_df_no_cut
    }

    # Process with_cut variant
    if (paste0(alias, "_cnt_wc") %in% names(df)) {
      type_df_with_cut <- df %>%
        dplyr::select(coord, alpha, delta,
                      count = !!rlang::sym(paste0(alias, "_cnt_wc")),
                      avg_prob = !!rlang::sym(paste0(alias, "_avg_wc")),
                      min_prob = !!rlang::sym(paste0(alias, "_min_wc")),
                      max_prob = !!rlang::sym(paste0(alias, "_max_wc")),
                      training_count = !!rlang::sym(paste0(alias, "_tr_wc"))) %>%
        dplyr::mutate(label = type, cut_type = "with_cut") %>%
        dplyr::filter(count > 0) %>%
        dplyr::select(label, cut_type, coord, alpha, delta, count, avg_prob, min_prob, max_prob, training_count)

      results[[paste0(type, "_with_cut")]] <- type_df_with_cut
    }
  }

  # Process merged "all_conditions" aggregates
  if ("all_cnt_nc" %in% names(df)) {
    all_no_cut <- df %>%
      dplyr::select(coord, alpha, delta,
                    count = all_cnt_nc,
                    training_count = all_tr_nc) %>%
      dplyr::mutate(label = "all_conditions", cut_type = "no_cut",
                    avg_prob = NA_real_, min_prob = NA_real_, max_prob = NA_real_) %>%
      dplyr::filter(count > 0) %>%
      dplyr::select(label, cut_type, coord, alpha, delta, count, avg_prob, min_prob, max_prob, training_count)

    results[["all_conditions_no_cut"]] <- all_no_cut
  }

  if ("all_cnt_wc" %in% names(df)) {
    all_with_cut <- df %>%
      dplyr::select(coord, alpha, delta,
                    count = all_cnt_wc,
                    training_count = all_tr_wc) %>%
      dplyr::mutate(label = "all_conditions", cut_type = "with_cut",
                    avg_prob = NA_real_, min_prob = NA_real_, max_prob = NA_real_) %>%
      dplyr::filter(count > 0) %>%
      dplyr::select(label, cut_type, coord, alpha, delta, count, avg_prob, min_prob, max_prob, training_count)

    results[["all_conditions_with_cut"]] <- all_with_cut
  }

  dplyr::bind_rows(results)
}


#' Build Skymap Aggregation Query
#'
#' Generates a SQL query to aggregate partial skymap results from a table
#' created by parallel execution. Properly handles weighted averages for
#' probability columns.
#'
#' @param table_name Character string. Fully-qualified table name containing
#'   partial results (schema.table).
#' @param type_conditions_list List. Output from \code{\link{parse_type_conditions}}.
#' @param alias_map Named list. Mapping from type identifiers to column aliases.
#'
#' @return A character string containing the aggregation SQL query.
#'
#' @details
#' The query computes:
#' \itemize{
#'   \item \strong{SUM} for count columns
#'   \item \strong{Weighted average} for probability averages:
#'     \code{SUM(avg * cnt) / NULLIF(SUM(cnt), 0)}
#'   \item \strong{MIN} for minimum probability columns
#'   \item \strong{MAX} for maximum probability columns
#' }
#'
#' Results are grouped by HEALPix coordinate and ordered for consistent output.
#'
#' @examples
#' \dontrun{
#' agg_query <- build_skymap_aggregation_query(
#'   "dr4_ops_cs48_mv.skymap_cepheidandrrlyrae_merged",
#'   type_conditions,
#'   alias_map
#' )
#' final_results <- dbGetQuery(conn, agg_query)
#' }
#'
#' @seealso \code{\link{build_merged_skymap_query}} for initial query generation
#'
#' @export
build_skymap_aggregation_query <- function(table_name, type_conditions_list, alias_map) {
  all_types <- type_conditions_list$types

  # Build SUM aggregates for each type
  sum_aggregates <- lapply(all_types, function(type) {
    alias <- alias_map[[type]]
    sprintf("
      SUM(%s_cnt_nc) AS %s_cnt_nc,
      SUM(%s_avg_nc * %s_cnt_nc) / NULLIF(SUM(%s_cnt_nc), 0) AS %s_avg_nc,
      MIN(%s_min_nc) AS %s_min_nc,
      MAX(%s_max_nc) AS %s_max_nc,
      SUM(%s_tr_nc) AS %s_tr_nc,
      SUM(%s_cnt_wc) AS %s_cnt_wc,
      SUM(%s_avg_wc * %s_cnt_wc) / NULLIF(SUM(%s_cnt_wc), 0) AS %s_avg_wc,
      MIN(%s_min_wc) AS %s_min_wc,
      MAX(%s_max_wc) AS %s_max_wc,
      SUM(%s_tr_wc) AS %s_tr_wc",
            alias, alias,
            alias, alias, alias, alias,
            alias, alias,
            alias, alias,
            alias, alias,
            alias, alias,
            alias, alias, alias, alias,
            alias, alias,
            alias, alias,
            alias, alias)
  })

  # Merged aggregates
  merged_aggregates <- "
      SUM(all_cnt_nc) AS all_cnt_nc,
      SUM(all_cnt_wc) AS all_cnt_wc,
      SUM(all_tr_nc) AS all_tr_nc,
      SUM(all_tr_wc) AS all_tr_wc"

  sprintf("
SELECT
  coord,
  degrees(coord[1]) AS alpha,
  degrees(coord[2]) AS delta,
  %s,
  %s
FROM %s
GROUP BY coord
ORDER BY coord",
          paste(unlist(sum_aggregates), collapse = ","),
          merged_aggregates,
          table_name)
}


#' Build Alias Legend
#'
#' Creates a documentation dataframe mapping column aliases to full type
#' identifiers for reference and debugging purposes.
#'
#' @param all_types Character vector. Full type identifiers.
#' @param alias_map Named list. Mapping from type identifiers to aliases.
#'
#' @return A dataframe with columns:
#'   \describe{
#'     \item{alias}{Character. Short column alias (c1, c2, etc.)}
#'     \item{full_type}{Character. Full type identifier (classifier:type)}
#'   }
#'
#' @examples
#' types <- c("CD_xgboost:CEP", "CD_xgboost:RR")
#' alias_map <- build_alias_map(types)
#' legend <- build_alias_legend(types, alias_map)
#' print(legend)
#' #   alias        full_type
#' # 1    c1   CD_xgboost:CEP
#' # 2    c2    CD_xgboost:RR
#'
#' @seealso \code{\link{build_alias_map}}
#'
#' @export
build_alias_legend <- function(all_types, alias_map) {
  legend <- data.frame(
    alias = sapply(all_types, function(t) alias_map[[t]]),
    full_type = all_types,
    stringsAsFactors = FALSE
  )
  return(legend)
}


# =============================================================================
# PARALLEL EXECUTION
# =============================================================================

#' Generate Parallel Execution Script
#'
#' Creates a shell script command for parallel SQL query execution using
#' the partParalXZ4 utility with Mattermost notifications.
#'
#' @param runid Integer. The run identifier.
#' @param table_name Character string. Target table name for results.
#' @param sql_query Character string. The SQL query to execute in parallel.
#' @param mm_user Character string. Mattermost username for notifications.
#'   Default is "\@nienarto".
#' @param parallelism Integer. Number of parallel workers. Default is 80.
#' @param num_chunks Integer. Number of data chunks to process. Default is 400.
#' @param description Character string. Job description for Mattermost notification.
#'
#' @return A character string containing the complete shell command.
#'
#' @details
#' The generated script uses heredoc syntax to embed the SQL query and
#' configures the partParalXZ4 utility with the specified parameters.
#' Results are written to a private schema and notifications are sent
#' to the cu7_classification Mattermost channel.
#'
#' @examples
#' \dontrun{
#' script <- generate_parallel_script(
#'   runid = 90005,
#'   table_name = "dr4_ops_cs48_mv.skymap_cepheidandrrlyrae",
#'   sql_query = "SELECT * FROM ...",
#'   parallelism = 80,
#'   num_chunks = 600
#' )
#' cat(script)
#' }
#'
#' @keywords internal
#' @export
generate_parallel_script <- function(runid, table_name, sql_query,
                                     mm_user = "@nienarto",
                                     parallelism = 80,
                                     num_chunks = 400,
                                     description = "Testing classif for sos inputs") {
  script <- sprintf("partParalXZ4 %s %s nienarto_local %s %d false private cu7_classification '%s' %d << 'EOF'\n---\n\n%s\n\n---\nEOF",
                    runid, table_name, mm_user, parallelism, description, num_chunks, sql_query)
  return(script)
}


#' Build Skymap Parallel Script
#'
#' Constructs a complete parallel execution package for skymap generation,
#' including the execution script, table name, alias mapping, and aggregation query.
#'
#' @param runid Integer. The run identifier.
#' @param type_conditions_list List. Output from \code{\link{parse_type_conditions}}.
#' @param schema Character string. Database schema. Default is "dr4_ops_cs48_mv".
#' @param mm_user Character string. Mattermost username. Default is "\@nienarto".
#' @param parallelism Integer. Number of parallel workers. Default is 80.
#' @param num_chunks Integer. Number of data chunks. Default is 600.
#'
#' @return A list containing:
#'   \describe{
#'     \item{script}{Character. Shell script for parallel execution}
#'     \item{table_name}{Character. Target table name}
#'     \item{alias_map}{Named list. Type to alias mapping}
#'     \item{alias_legend}{Dataframe. Human-readable alias documentation}
#'     \item{aggregation_query}{Character. SQL query to aggregate partial results}
#'   }
#'
#' @examples
#' \dontrun{
#' type_conditions <- parse_type_conditions(config[1, ])
#' skymap_pkg <- build_skymap_parallel_script(90005, type_conditions)
#'
#' # View the execution script
#' cat(skymap_pkg$script)
#'
#' # Check alias mappings
#' print(skymap_pkg$alias_legend)
#'
#' # Execute the aggregation after parallel job completes
#' final_results <- dbGetQuery(conn, skymap_pkg$aggregation_query)
#' }
#'
#' @seealso
#' \code{\link{build_merged_skymap_query}},
#' \code{\link{execute_parallel_scripts}}
#'
#' @export
build_skymap_parallel_script <- function(runid, type_conditions_list, schema = "dr4_ops_cs48_mv",
                                         mm_user = "@nienarto", parallelism = 80, num_chunks = 600) {

  table_name <- build_table_name(schema, "skymap", type_conditions_list$sosname, suffix = "merged")

  query_result <- build_merged_skymap_query(runid, type_conditions_list, schema)
  sql_query <- query_result$query
  alias_map <- query_result$alias_map

  script <- generate_parallel_script(
    runid = runid,
    table_name = table_name,
    sql_query = sql_query,
    mm_user = mm_user,
    parallelism = parallelism,
    num_chunks = num_chunks,
    description = sprintf("Skymap merged for %s", type_conditions_list$sosname)
  )

  # Build legend for reference
  legend <- build_alias_legend(type_conditions_list$types, alias_map)

  list(
    script = script,
    table_name = table_name,
    alias_map = alias_map,
    alias_legend = legend,
    aggregation_query = build_skymap_aggregation_query(table_name, type_conditions_list, alias_map)
  )
}


#' Execute Parallel Scripts Sequentially
#'
#' Executes a list of parallel script packages, optionally running aggregation
#' queries after each script completes.
#'
#' @param scripts Named list. Script packages from \code{\link{build_skymap_parallel_script}}
#'   or similar functions.
#' @param conn DBI connection or NULL. Database connection for running aggregation
#'   queries. If NULL, aggregation is skipped.
#' @param execute Logical. If TRUE, actually execute the scripts. If FALSE,
#'   return script information without execution. Default is FALSE.
#' @param debug Logical. If TRUE, print detailed information about each script.
#'   Default is FALSE.
#'
#' @return A named list with results for each script:
#'   \describe{
#'     \item{success}{Logical. Whether execution succeeded (only if execute=TRUE)}
#'     \item{exit_code}{Integer. Shell exit code (only if execution failed)}
#'     \item{table_name}{Character. Target table name}
#'     \item{alias_map}{Named list. Type to alias mapping}
#'     \item{alias_legend}{Dataframe. Alias documentation}
#'     \item{aggregated_data}{Dataframe. Results of aggregation query
#'       (only if conn provided and execute=TRUE)}
#'   }
#'
#' @details
#' When \code{execute = FALSE}, the function returns all script information
#' without running anything, useful for inspection and debugging.
#'
#' When \code{execute = TRUE}, scripts are run sequentially using \code{system()}.
#' If a database connection is provided, the aggregation query is executed
#' after each parallel job completes successfully.
#'
#' @examples
#' \dontrun{
#' # Preview scripts without execution
#' scripts <- list(skymap = build_skymap_parallel_script(90005, type_conditions))
#' preview <- execute_parallel_scripts(scripts, execute = FALSE, debug = TRUE)
#'
#' # Execute with aggregation
#' results <- execute_parallel_scripts(scripts, conn = conn, execute = TRUE)
#' final_data <- results$skymap$aggregated_data
#' }
#'
#' @export
execute_parallel_scripts <- function(scripts, conn = NULL, execute = FALSE, debug = FALSE) {
  results <- list()

  for (i in seq_along(scripts)) {
    script_info <- scripts[[i]]
    script_name <- names(scripts)[i]

    if (debug) {
      cat(sprintf("\n=== SCRIPT %d: %s ===\n", i, script_name))
      cat(sprintf("Table: %s\n", script_info$table_name))
      cat("\n--- Alias Legend ---\n")
      print(script_info$alias_legend)
      cat("\n--- Script ---\n")
      cat(script_info$script, "\n")
      cat("\n--- Aggregation Query ---\n")
      cat(script_info$aggregation_query, "\n")
      cat("==================\n\n")
    }

    if (execute) {
      cat(sprintf("Executing script %d/%d: %s\n", i, length(scripts), script_name))

      exit_code <- system(script_info$script, intern = FALSE)

      if (exit_code != 0) {
        warning(sprintf("Script %s failed with exit code %d", script_name, exit_code))
        results[[script_name]] <- list(success = FALSE, exit_code = exit_code)
      } else {
        results[[script_name]] <- list(
          success = TRUE,
          table_name = script_info$table_name,
          alias_map = script_info$alias_map,
          alias_legend = script_info$alias_legend
        )

        if (!is.null(conn)) {
          cat(sprintf("Executing aggregation for %s...\n", script_name))
          agg_result <- dbGetQuery(conn, script_info$aggregation_query)
          results[[script_name]]$aggregated_data <- agg_result
        }
      }
    } else {
      results[[script_name]] <- list(
        script = script_info$script,
        table_name = script_info$table_name,
        alias_map = script_info$alias_map,
        alias_legend = script_info$alias_legend,
        aggregation_query = script_info$aggregation_query
      )
    }
  }

  return(results)
}


# =============================================================================
# MAIN WORKFLOW
# =============================================================================

#' Run Classifier Analysis Pipeline
#'
#' Main entry point for the classifier analysis workflow. Extracts configuration,
#' builds parallel scripts, and optionally executes them with result aggregation.
#'
#' @param inparams List. Database connection parameters containing:
#'   \describe{
#'     \item{hostname}{Database server hostname}
#'     \item{dbPort}{Database port number}
#'     \item{dbUser}{Database username}
#'   }
#' @param runid Integer. The run identifier to analyze.
#' @param sosname_conf Character string. SOS algorithm configuration identifier
#'   (e.g., "sos_ceprrl_conf").
#' @param schema Character string. Database schema. Default is "dr4_ops_cs48_mv".
#' @param mm_user Character string. Mattermost username for notifications.
#'   Default is "\@nienarto".
#' @param parallelism Integer. Number of parallel workers. Default is 80.
#' @param num_chunks Integer. Number of data chunks for parallel processing.
#'   Default is 600.
#' @param execute Logical. If TRUE, execute parallel scripts. If FALSE, only
#'   generate scripts. Default is FALSE.
#' @param debug Logical. If TRUE, print detailed debug information. Default is FALSE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{skymap}{Script package and results for skymap generation}
#'     \item{skymap_pivoted}{Dataframe. Pivoted skymap results (only if execute=TRUE
#'       and aggregation succeeds)}
#'     \item{metadata}{List with analysis metadata:
#'       \itemize{
#'         \item runid: Run identifier
#'         \item sosname_conf: Configuration identifier
#'         \item sosname: SOS name
#'         \item n_types: Number of classifier types
#'         \item types: Character vector of all types
#'         \item cuts: Nested list of probability thresholds
#'       }
#'     }
#'   }
#'
#' @details
#' The function performs the following workflow:
#' \enumerate{
#'   \item Connect to database using provided parameters
#'   \item Extract classifier configuration for the specified run and SOS
#'   \item Parse configuration into structured type conditions
#'   \item Build parallel execution scripts for skymap generation
#'   \item Optionally execute scripts and aggregate results
#'   \item Pivot results into analysis-ready format
#' }
#'
#' @section Execution Modes:
#' \describe{
#'   \item{Preview mode (execute=FALSE)}{Generates all scripts and queries without
#'     execution. Useful for reviewing SQL, testing configuration, and debugging.}
#'   \item{Execution mode (execute=TRUE)}{Runs parallel scripts sequentially,
#'     executes aggregation queries, and pivots results for analysis.}
#' }
#'
#' @examples
#' \dontrun{
#' # Define connection parameters
#' inparams <- list(
#'   hostname = "gaia-db.example.com",
#'   dbPort = 5432,
#'   dbUser = "analyst"
#' )
#'
#' # Preview mode - generate scripts without execution
#' results <- run_classifier_analysis(
#'   inparams,
#'   runid = 90005,
#'   sosname_conf = "sos_ceprrl_conf",
#'   execute = FALSE,
#'   debug = TRUE
#' )
#'
#' # View generated script
#' cat(results$skymap$script)
#'
#' # Execution mode - run full analysis
#' results <- run_classifier_analysis(
#'   inparams,
#'   runid = 90005,
#'   sosname_conf = "sos_ceprrl_conf",
#'   execute = TRUE,
#'   parallelism = 80,
#'   num_chunks = 600
#' )
#'
#' # Access pivoted results
#' head(results$skymap_pivoted)
#'
#' # Check metadata
#' results$metadata$types
#' results$metadata$cuts
#' }
#'
#' @seealso
#' \code{\link{extract_classifier_config}} for configuration extraction,
#' \code{\link{parse_type_conditions}} for configuration parsing,
#' \code{\link{build_skymap_parallel_script}} for script generation,
#' \code{\link{pivot_skymap_results}} for result transformation
#'
#' @export
run_classifier_analysis <- function(inparams, runid, sosname_conf,
                                    schema = "dr4_ops_cs48_mv",
                                    mm_user = "@nienarto",
                                    parallelism = 80,
                                    num_chunks = 600,
                                    execute = FALSE,
                                    debug = FALSE) {

  conn <- dpcgR::connect(hostname = inparams$hostname, port = inparams$dbPort, user = inparams$dbUser)

  cat("Extracting classifier configuration...\n")
  config <- extract_classifier_config(conn, runid, sosname_conf)

  if (nrow(config) == 0) {
    DBI::dbDisconnect(conn)
    stop("No configuration found")
  }

  cat("Parsing type conditions...\n")
  type_conditions <- parse_type_conditions(config[1, ])

  cat(sprintf("Found %d types: %s\n", length(type_conditions$types),
              paste(type_conditions$types, collapse = ", ")))

  cat("Building parallel scripts...\n")

  skymap_script <- build_skymap_parallel_script(
    runid = runid,
    type_conditions_list = type_conditions,
    schema = schema,
    mm_user = mm_user,
    parallelism = parallelism,
    num_chunks = num_chunks
  )

  scripts <- list(
    skymap = skymap_script
  )

  if (execute) {
    cat("Executing parallel scripts sequentially...\n")
    results <- execute_parallel_scripts(scripts, conn = conn, execute = TRUE, debug = debug)

    if (!is.null(results$skymap$aggregated_data) && !is.null(results$skymap$alias_map)) {
      results$skymap_pivoted <- pivot_skymap_results(
        results$skymap$aggregated_data,
        type_conditions$types,
        results$skymap$alias_map
      )
    }
  } else {
    results <- execute_parallel_scripts(scripts, conn = NULL, execute = FALSE, debug = debug)
  }

  DBI::dbDisconnect(conn)

  cat("Analysis complete!\n")

  results$metadata <- list(
    runid = runid,
    sosname_conf = sosname_conf,
    sosname = type_conditions$sosname,
    n_types = length(type_conditions$types),
    types = type_conditions$types,
    cuts = type_conditions$cuts
  )

  return(results)
}
