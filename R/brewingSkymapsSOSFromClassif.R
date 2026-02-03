#' @title Skymap Plotting Functions for Pivoted Data
#' @description
#' Functions for plotting skymaps from pivoted classifier analysis data,
#' including Aitoff-Galactic projections, statistics calculation, and
#' batch processing of multiple label/cut_type combinations.
#'
#' @name skymap_plotting

NULL

#' Calculate Summary Statistics for Skymap Data
#'
#' Computes summary statistics for a filtered skymap dataset including
#' total counts and training source counts. Handles integer64 conversion
#' from PostgreSQL BIGINT columns.
#'
#' @param skymap_data A dataframe containing skymap data with count and
#'   training_count columns. Should already be filtered by label and cut_type.
#' @param label_name Character. The label being processed (for logging/debugging).
#' @param cut_type Character. The cut type (for logging/debugging).
#'
#' @return A list with summary statistics:
#'   \describe{
#'     \item{total_count}{Numeric. Sum of all source counts}
#'     \item{total_training}{Numeric. Sum of all training source counts}
#'     \item{n_pixels}{Integer. Number of HEALPix pixels with data}
#'   }
#'
#' @examples
#' \dontrun{
#' # Filter data for a specific label/cut combination
#' filtered <- skymap_pivoted %>%
#'   filter(label == "CEPHEID", cut_type == "with_cut")
#'
#' # Calculate statistics
#' stats <- calculate_skymap_stats(filtered, "CEPHEID", "with_cut")
#' cat(sprintf("Total sources: %d\n", stats$total_count))
#' }
#'
#' @keywords internal
calculate_skymap_stats <- function(skymap_data, label_name = NULL, cut_type = NULL) {
  stats <- list(
    total_count = 0,
    total_training = 0,
    n_pixels = 0
  )

  if (is.null(skymap_data) || nrow(skymap_data) == 0) {
    return(stats)
  }

  stats$n_pixels <- nrow(skymap_data)

  if ("count" %in% names(skymap_data)) {
    # Ensure numeric conversion for integer64
    count_vals <- as.numeric(skymap_data$count)
    stats$total_count <- sum(count_vals, na.rm = TRUE)
  }

  if ("training_count" %in% names(skymap_data)) {
    training_vals <- as.numeric(skymap_data$training_count)
    stats$total_training <- sum(training_vals, na.rm = TRUE)
  }

  return(stats)
}


#' Calculate Summary Statistics for All Conditions (Merged)
#'
#' For "all_conditions" labels, calculates totals by summing from individual
#' type data rather than using pre-aggregated values which may be incorrect
#' due to source overlap between classifiers.
#'
#' @param full_pivoted_data Dataframe. The complete pivoted skymap data
#'   containing all labels and cut types.
#' @param cut_type Character. The cut type to filter by ("with_cut" or "no_cut").
#'
#' @return A list with summary statistics:
#'   \describe{
#'     \item{total_count}{Numeric. Sum of all source counts across individual types}
#'     \item{total_training}{Numeric. Sum of all training source counts}
#'     \item{n_pixels}{Integer. Number of unique HEALPix pixels}
#'   }
#'
#' @details
#' This function is specifically designed for the "all_conditions" merged view,
#' where statistics should reflect the sum of individual classifier results
#' rather than a simple aggregation which may double-count sources.
#'
#' @keywords internal
calculate_merged_stats <- function(full_pivoted_data, cut_type) {
  stats <- list(
    total_count = 0,
    total_training = 0,
    n_pixels = 0
  )

  if (is.null(full_pivoted_data) || nrow(full_pivoted_data) == 0) {
    return(stats)
  }

  # Get all individual type data (exclude all_conditions)
  individual_data <- full_pivoted_data %>%
    dplyr::filter(label != "all_conditions", cut_type == !!cut_type)

  if (nrow(individual_data) > 0) {
    if ("count" %in% names(individual_data)) {
      stats$total_count <- sum(as.numeric(individual_data$count), na.rm = TRUE)
    }
    if ("training_count" %in% names(individual_data)) {
      stats$total_training <- sum(as.numeric(individual_data$training_count), na.rm = TRUE)
    }
    # For pixels, count unique coordinates
    if ("coord" %in% names(individual_data)) {
      stats$n_pixels <- length(unique(individual_data$coord))
    } else {
      stats$n_pixels <- nrow(individual_data)
    }
  }

  return(stats)
}


#' Build Subtitle with Statistics
#'
#' Creates a formatted subtitle string containing source counts and metadata
#' for skymap plot annotations.
#'
#' @param label_name Character. The label/type name being plotted.
#' @param cut_type Character. The cut type ("with_cut" or "no_cut").
#' @param stats List. Statistics from \code{\link{calculate_skymap_stats}} or
#'   \code{\link{calculate_merged_stats}}.
#' @param column_name Character. The column being plotted (optional, for context).
#'
#' @return A character string formatted as a subtitle with parts separated by " | ",
#'   or NULL if no statistics are available.
#'
#' @examples
#' \dontrun{
#' stats <- list(total_count = 12345, total_training = 500, n_pixels = 1000)
#' subtitle <- build_skymap_subtitle("CEPHEID", "with_cut", stats)
#' # Returns: "Sources: 12,345 | Training: 500 | With Cuts"
#' }
#'
#' @keywords internal
build_skymap_subtitle <- function(label_name, cut_type, stats, column_name = NULL) {
  parts <- c()

  # Add total sources count
  if (!is.null(stats$total_count) && stats$total_count > 0) {
    parts <- c(parts, sprintf("Sources: %s", format(stats$total_count, big.mark = ",")))
  }

  # Add training count
  if (!is.null(stats$total_training) && stats$total_training > 0) {
    parts <- c(parts, sprintf("Training: %s", format(stats$total_training, big.mark = ",")))
  }

  # Add cut type info
  cut_display <- if (!is.null(cut_type)) {
    ifelse(cut_type == "with_cut", "With Cuts", "No Cuts")
  } else {
    NULL
  }

  if (!is.null(cut_display)) {
    parts <- c(parts, cut_display)
  }

  if (length(parts) > 0) {
    return(paste(parts, collapse = " | "))
  } else {
    return(NULL)
  }
}


#' Get Valid Columns for Label
#'
#' Returns the list of columns that should be plotted for a given label.
#' For "all_conditions", probability-based columns are excluded since they
#' cannot be meaningfully aggregated across different classifiers.
#'
#' @param label_name Character. The label name to check.
#' @param requested_columns Character vector. Columns requested for plotting.
#' @param available_columns Character vector. Columns actually available in the data.
#'
#' @return Character vector of valid columns to plot, filtered to exclude
#'   probability columns for "all_conditions" and non-existent columns.
#'
#' @details
#' Probability columns (avg_prob, min_prob, max_prob) are NA for "all_conditions"
#' because averaging probabilities across different classifiers with different
#' scales and meanings is not statistically valid.
#'
#' @examples
#' \dontrun{
#' # For individual types, all columns are valid
#' get_valid_columns_for_label("CEPHEID",
#'   c("count", "avg_prob", "min_prob"),
#'   c("count", "avg_prob", "min_prob", "training_count"))
#' # Returns: c("count", "avg_prob", "min_prob")
#'
#' # For all_conditions, probability columns are excluded
#' get_valid_columns_for_label("all_conditions",
#'   c("count", "avg_prob", "min_prob"),
#'   c("count", "avg_prob", "min_prob", "training_count"))
#' # Returns: c("count")
#' }
#'
#' @keywords internal
get_valid_columns_for_label <- function(label_name, requested_columns, available_columns) {
  # Probability columns that are NA for all_conditions
  prob_columns <- c("avg_prob", "min_prob", "max_prob")

  # For all_conditions, exclude probability columns
  if (label_name == "all_conditions") {
    valid_columns <- requested_columns[!requested_columns %in% prob_columns]
  } else {
    valid_columns <- requested_columns
  }

  # Filter to only columns that exist in the data
  valid_columns <- valid_columns[valid_columns %in% available_columns]

  return(valid_columns)
}


#' Plot Single Skymap from Pivoted Data
#'
#' Creates a single Aitoff-Galactic projection skymap for a specific
#' label/cut_type/column combination from pivoted skymap data.
#'
#' @param skymap_data Dataframe. Pre-filtered skymap data for a single
#'   label/cut_type combination. Must contain alpha, delta columns and
#'   the specified column_name.
#' @param label_name Character. The label/type name (e.g., "CEPHEID", "RR_LYRAE").
#' @param cut_type Character. The cut type ("with_cut" or "no_cut").
#' @param column_name Character. The column to plot (e.g., "count", "avg_prob").
#' @param runid Integer. Optional run ID for title annotation.
#' @param hpxLevel Integer. HEALPix level for the skymap (default 8).
#' @param use_log_scale Logical. Whether to use logarithmic color scale (default TRUE).
#'   Automatically disabled if data contains zeros or negative values.
#' @param output_dir Character. Optional directory path to save plot as PNG.
#' @param prep_mattermost Logical. Whether to prepare a temporary file for
#'   Mattermost upload (default FALSE).
#' @param stats List. Optional pre-calculated statistics from
#'   \code{\link{calculate_skymap_stats}}. If NULL, statistics are calculated
#'   from the provided data.
#'
#' @return A list containing:
#'   \describe{
#'     \item{plot}{ggplot2 object of the skymap}
#'     \item{temp_file}{Path to temporary PNG file (if prep_mattermost = TRUE)}
#'     \item{saved_file}{Path to saved PNG file (if output_dir specified)}
#'     \item{plot_title}{Character string of the plot title}
#'     \item{stats}{Statistics used for the subtitle}
#'   }
#'   Returns list with NULL values if plotting fails or no valid data.
#'
#' @details
#' The function handles several edge cases:
#' \itemize{
#'   \item Converts integer64 columns to numeric
#'   \item Automatically switches to linear scale for data with zeros/negatives
#'   \item Skips plotting if column doesn't exist or has no valid data
#'   \item Creates output directory if it doesn't exist
#' }
#'
#' @seealso \code{\link{plot_all_skymaps_pivoted}} for batch plotting,
#'   \code{\link{plotAitoffGalactic}} for the underlying plot function
#'
#' @examples
#' \dontrun{
#' # Filter data for one combination
#' filtered <- skymap_pivoted %>%
#'   filter(label == "CEPHEID", cut_type == "with_cut")
#'
#' # Create single plot
#' result <- plot_skymap_pivoted(
#'   skymap_data = filtered,
#'   label_name = "CEPHEID",
#'   cut_type = "with_cut",
#'   column_name = "count",
#'   runid = 90005
#' )
#'
#' # Display the plot
#' print(result$plot)
#' }
#'
#' @export
plot_skymap_pivoted <- function(skymap_data,
                                label_name = NULL,
                                cut_type = NULL,
                                column_name,
                                runid = NULL,
                                hpxLevel = 8,
                                use_log_scale = TRUE,
                                output_dir = NULL,
                                prep_mattermost = FALSE,
                                stats = NULL) {

  # Build title components
  runid_str <- if (!is.null(runid)) sprintf(" [runid:%s]", runid) else ""
  label_str <- if (!is.null(label_name)) label_name else "All Labels"
  cut_str <- if (!is.null(cut_type)) cut_type else "All Cuts"

  title_prefix <- sprintf("%s - %s%s", label_str, cut_str, runid_str)

  if (is.null(skymap_data) || nrow(skymap_data) == 0) {
    warning(sprintf("No data available for: %s", title_prefix))
    return(NULL)
  }

  # Use provided stats or calculate from data
  if (is.null(stats)) {
    stats <- calculate_skymap_stats(skymap_data)
  }

  # Build subtitle with statistics
  subtitle_text <- build_skymap_subtitle(label_name, cut_type, stats, column_name)

  # Check if column exists
  if (!column_name %in% names(skymap_data)) {
    message(sprintf("    Column '%s' not available for: %s (skipping)", column_name, label_str))
    return(NULL)
  }

  # Convert integer64 to numeric if needed
  if (inherits(skymap_data[[column_name]], "integer64")) {
    skymap_data[[column_name]] <- as.numeric(skymap_data[[column_name]])
  }

  # Check data quality
  col_data <- skymap_data[[column_name]]
  col_data_clean <- col_data[!is.na(col_data)]

  if (length(col_data_clean) == 0) {
    message(sprintf("    No valid data in '%s' for: %s (skipping)", column_name, label_str))
    return(NULL)
  }

  # Determine if log scale should be used
  has_negatives <- any(col_data_clean < 0)
  has_zeros <- any(col_data_clean == 0)
  has_positive <- any(col_data_clean > 0)

  use_log_final <- use_log_scale
  if (use_log_scale && (has_negatives || has_zeros || !has_positive)) {
    use_log_final <- FALSE
    if (has_negatives) {
      message(sprintf("    Negative values in '%s', using linear scale", column_name))
    } else if (has_zeros) {
      message(sprintf("    Zero values in '%s', using linear scale", column_name))
    }
  }

  # Create plot
  tryCatch({
    title_name <- paste0(title_prefix, " - ", column_name)
    legend_name <- column_name

    plot_args <- list(
      skyMapD = skymap_data,
      valueName = column_name,
      titleName = title_name,
      legendName = legend_name,
      subtitleName = subtitle_text,
      hpxLevel = hpxLevel,
      use_log_scale = use_log_final
    )

    p <- do.call(plotAitoffGalactic, plot_args)

    # Prepare result object
    result <- list(
      plot = p,
      temp_file = NULL,
      saved_file = NULL,
      plot_title = title_name,
      stats = stats
    )

    # Save plot if output directory specified
    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      label_safe <- gsub("[^a-zA-Z0-9_-]", "_", label_str)
      cut_safe <- gsub("[^a-zA-Z0-9_-]", "_", cut_str)

      filename <- if (!is.null(runid)) {
        sprintf("%s_%s_%s_runid%s.png", label_safe, cut_safe, column_name, runid)
      } else {
        sprintf("%s_%s_%s.png", label_safe, cut_safe, column_name)
      }

      filepath <- file.path(output_dir, filename)
      ggplot2::ggsave(filepath, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
      cat(sprintf("    Saved plot to: %s\n", filepath))
      result$saved_file <- filepath
    }

    # Prepare temp file for Mattermost if requested
    if (prep_mattermost) {
      safe_filename <- gsub("[^a-zA-Z0-9_-]", "_", title_name)
      safe_filename <- gsub("_{2,}", "_", safe_filename)
      temp_file <- tempfile(pattern = paste0(safe_filename, "_"), fileext = ".png")
      ggplot2::ggsave(temp_file, plot = p, width = 10, height = 6, dpi = 150, bg = "white")
      result$temp_file <- temp_file
    }

    return(result)

  }, error = function(e) {
    warning(sprintf("Error plotting '%s' for %s: %s", column_name, title_prefix, e$message))
    return(list(plot = NULL, temp_file = NULL, saved_file = NULL, plot_title = NULL, stats = NULL))
  })
}


#' Sort Skymap Combinations for Display
#'
#' Sorts label/cut_type combinations to ensure consistent and logical
#' ordering for batch plotting and display.
#'
#' @param unique_combos A dataframe with label and cut_type columns.
#'
#' @return A sorted dataframe with the same structure, ordered by:
#'   \enumerate{
#'     \item all_conditions with_cut first
#'     \item all_conditions no_cut second
#'     \item Other labels sorted alphabetically, with_cut before no_cut
#'   }
#'
#' @details
#' This ordering prioritizes the merged "all_conditions" view which gives
#' an overview, followed by individual classifier results in alphabetical
#' order for easy navigation.
#'
#' @keywords internal
sort_skymap_combinations <- function(unique_combos) {
  sorted_combos <- unique_combos %>%
    dplyr::mutate(
      is_all_conditions = ifelse(label == "all_conditions", 0, 1),
      cut_priority = ifelse(cut_type == "with_cut", 0, 1),
      label_lower = tolower(label)
    ) %>%
    dplyr::arrange(is_all_conditions, cut_priority, label_lower) %>%
    dplyr::select(-is_all_conditions, -cut_priority, -label_lower)

  return(sorted_combos)
}


#' Plot All Skymaps from Pivoted Data
#'
#' Batch processes all unique label/cut_type combinations in pivoted skymap
#' data, generating Aitoff-Galactic projection plots for each combination
#' and specified columns.
#'
#' @param skymap_pivoted Dataframe. Pivoted skymap data with columns:
#'   label, cut_type, alpha, delta, coord, count, avg_prob, min_prob, max_prob,
#'   training_count, etc.
#' @param runid Integer. Optional run ID for title annotation.
#' @param columns_to_plot Character vector. Columns to plot for each combination
#'   (default: c("count", "avg_prob", "min_prob", "max_prob")).
#'   Probability columns are automatically excluded for "all_conditions".
#' @param plot_training_comparison Logical. Whether to create count/training_count
#'   ratio plots (default TRUE).
#' @param hpxLevel Integer. HEALPix level for skymaps (default 8).
#' @param use_log_scale Logical. Whether to use logarithmic color scale (default TRUE).
#' @param output_dir Character. Optional directory path to save plots as PNG files.
#' @param show_plots Logical. Whether to print plots to the graphics device (default TRUE).
#' @param send_to_mattermost Logical. Whether to send plots to Mattermost (default FALSE).
#' @param mattermost_config List. Mattermost configuration with elements:
#'   url, token, team, channel.
#' @param mattermost_message_prefix Character. Optional message prefix for Mattermost posts.
#'
#' @return Invisibly returns a named list of all generated ggplot2 objects.
#'   Names are formatted as "label_cuttype_column".
#'
#' @details
#' Processing order:
#' \enumerate{
#'   \item all_conditions with_cut (overview with probability cuts applied)
#'   \item all_conditions no_cut (overview without cuts)
#'   \item Individual labels alphabetically, with_cut before no_cut
#' }
#'
#' For each combination, the function:
#' \itemize{
#'   \item Calculates appropriate statistics (merged for all_conditions)
#'   \item Filters valid columns (excludes prob columns for all_conditions)
#'   \item Creates plots for each valid column
#'   \item Optionally creates count/training_count ratio plots
#'   \item Optionally saves to files and/or sends to Mattermost
#' }
#'
#' @seealso \code{\link{plot_skymap_pivoted}} for single plot creation,
#'   \code{\link{pivot_skymap_results}} for data preparation
#'
#' @examples
#' \dontrun{
#' # Basic usage - display all plots
#' all_plots <- plot_all_skymaps_pivoted(
#'   skymap_pivoted = my_pivoted_data,
#'   runid = 90005,
#'   show_plots = TRUE
#' )
#'
#' # Save to files without displaying
#' all_plots <- plot_all_skymaps_pivoted(
#'   skymap_pivoted = my_pivoted_data,
#'   runid = 90005,
#'   output_dir = "output/skymaps",
#'   show_plots = FALSE
#' )
#'
#' # Custom columns, no training comparison
#' all_plots <- plot_all_skymaps_pivoted(
#'   skymap_pivoted = my_pivoted_data,
#'   columns_to_plot = c("count", "avg_prob"),
#'   plot_training_comparison = FALSE
#' )
#'
#' # Send to Mattermost
#' all_plots <- plot_all_skymaps_pivoted(
#'   skymap_pivoted = my_pivoted_data,
#'   send_to_mattermost = TRUE,
#'   mattermost_config = list(
#'     url = "https://mattermost.example.com",
#'     token = "your_token",
#'     team = "your_team",
#'     channel = "your_channel"
#'   ),
#'   mattermost_message_prefix = "Daily classifier analysis"
#' )
#' }
#'
#' @export
plot_all_skymaps_pivoted <- function(skymap_pivoted,
                                     runid = NULL,
                                     columns_to_plot = c("count", "avg_prob", "min_prob", "max_prob"),
                                     plot_training_comparison = TRUE,
                                     hpxLevel = 8,
                                     use_log_scale = TRUE,
                                     output_dir = NULL,
                                     show_plots = TRUE,
                                     send_to_mattermost = FALSE,
                                     mattermost_config = NULL,
                                     mattermost_message_prefix = NULL) {

  all_plots <- list()
  temp_files <- c()
  plot_titles <- c()

  # Get unique combinations of label and cut_type
  unique_combos <- skymap_pivoted %>%
    dplyr::select(label, cut_type) %>%
    dplyr::distinct()

  # Sort combinations: all_conditions_with_cut first, then all_conditions_no_cut, then others
  unique_combos <- sort_skymap_combinations(unique_combos)

  cat(sprintf("\n=== Processing %d unique label/cut_type combinations ===\n", nrow(unique_combos)))
  cat("Order of processing:\n")
  for (i in 1:nrow(unique_combos)) {
    cat(sprintf("  %d. %s - %s\n", i, unique_combos$label[i], unique_combos$cut_type[i]))
  }
  cat("\n")

  # Process each combination
  for (i in 1:nrow(unique_combos)) {
    label_name <- unique_combos$label[i]
    cut_type_val <- unique_combos$cut_type[i]

    cat(sprintf("\n[%d/%d] Processing: Label='%s', Cut='%s'\n",
                i, nrow(unique_combos), label_name, cut_type_val))

    # Filter data for this combination - be explicit about the filter
    filtered_data <- skymap_pivoted %>%
      dplyr::filter(label == !!label_name, cut_type == !!cut_type_val)

    if (nrow(filtered_data) == 0) {
      cat("  No data for this combination\n")
      next
    }

    # Debug: verify the filtering worked
    cat(sprintf("  DEBUG: Filtered %d rows for label='%s', cut_type='%s'\n",
                nrow(filtered_data), label_name, cut_type_val))

    # Calculate stats - use different method for all_conditions
    if (label_name == "all_conditions") {
      stats <- calculate_merged_stats(skymap_pivoted, cut_type_val)
    } else {
      stats <- calculate_skymap_stats(filtered_data, label_name, cut_type_val)
    }

    cat(sprintf("  Sources: %s | Training: %s | Pixels: %s\n",
                format(stats$total_count, big.mark = ","),
                format(stats$total_training, big.mark = ","),
                format(stats$n_pixels, big.mark = ",")))

    # Get valid columns for this label (excludes prob columns for all_conditions)
    valid_columns <- get_valid_columns_for_label(
      label_name,
      columns_to_plot,
      names(filtered_data)
    )

    # Ensure "count" is first if present
    if ("count" %in% valid_columns) {
      valid_columns <- c("count", valid_columns[valid_columns != "count"])
    }

    if (length(valid_columns) == 0) {
      cat("  No valid columns to plot for this label\n")
      next
    }

    cat(sprintf("  Columns to plot: %s\n", paste(valid_columns, collapse = ", ")))

    # Plot each valid column
    for (col in valid_columns) {
      cat(sprintf("  Plotting: %s\n", col))

      result <- plot_skymap_pivoted(
        skymap_data = filtered_data,
        label_name = label_name,
        cut_type = cut_type_val,
        column_name = col,
        runid = runid,
        hpxLevel = hpxLevel,
        use_log_scale = use_log_scale,
        output_dir = output_dir,
        prep_mattermost = send_to_mattermost && !is.null(mattermost_config),
        stats = stats
      )

      if (!is.null(result$plot)) {
        plot_name <- sprintf("%s_%s_%s",
                             gsub("[^a-zA-Z0-9_-]", "_", label_name),
                             gsub("[^a-zA-Z0-9_-]", "_", cut_type_val),
                             col)
        all_plots[[plot_name]] <- result$plot
        if (show_plots) print(result$plot)

        if (!is.null(result$temp_file)) {
          temp_files <- c(temp_files, result$temp_file)
          plot_titles <- c(plot_titles, result$plot_title)
        }
      }
    }

    # Plot training_count vs count comparison if requested
    if (plot_training_comparison &&
        "training_count" %in% names(filtered_data) &&
        "count" %in% names(filtered_data)) {

      cat("  Creating count/training ratio plot\n")

      # Convert integer64 to numeric and calculate ratio
      comparison_data <- filtered_data %>%
        dplyr::mutate(
          training_count_num = as.numeric(training_count),
          count_num = as.numeric(count),
          count_ratio = dplyr::case_when(
            training_count_num > 0 & count_num > 0 ~ count_num / training_count_num,
            training_count_num == 0 & count_num > 0 ~ Inf,
            TRUE ~ NA_real_
          )
        )

      # Check if we have valid ratio data
      n_valid_ratios <- sum(!is.na(comparison_data$count_ratio) & is.finite(comparison_data$count_ratio))
      n_inf_ratios <- sum(is.infinite(comparison_data$count_ratio))

      cat(sprintf("    Valid ratios: %d, Infinite: %d, NA: %d\n",
                  n_valid_ratios, n_inf_ratios, sum(is.na(comparison_data$count_ratio))))

      if (n_valid_ratios > 0) {
        # For plotting, cap infinite values at a high number
        comparison_data_plot <- comparison_data %>%
          dplyr::mutate(count_ratio = ifelse(is.infinite(count_ratio),
                                             max(count_ratio[is.finite(count_ratio)], na.rm = TRUE) * 10,
                                             count_ratio))

        # Plot ratio
        result_ratio <- plot_skymap_pivoted(
          skymap_data = comparison_data_plot,
          label_name = label_name,
          cut_type = cut_type_val,
          column_name = "count_ratio",
          runid = runid,
          hpxLevel = hpxLevel,
          use_log_scale = TRUE,
          output_dir = output_dir,
          prep_mattermost = send_to_mattermost && !is.null(mattermost_config),
          stats = stats
        )

        if (!is.null(result_ratio$plot)) {
          plot_name <- sprintf("%s_%s_count_ratio",
                               gsub("[^a-zA-Z0-9_-]", "_", label_name),
                               gsub("[^a-zA-Z0-9_-]", "_", cut_type_val))
          all_plots[[plot_name]] <- result_ratio$plot
          if (show_plots) print(result_ratio$plot)

          if (!is.null(result_ratio$temp_file)) {
            temp_files <- c(temp_files, result_ratio$temp_file)
            plot_titles <- c(plot_titles, result_ratio$plot_title)
          }
        }
      } else {
        cat("    No valid ratio data to plot\n")
      }
    }
  }

  cat(sprintf("\n=== Completed plotting %d skymaps ===\n", length(all_plots)))

  # Send to Mattermost if requested
  if (send_to_mattermost && !is.null(mattermost_config) && length(temp_files) > 0) {
    cat(sprintf("\nSending %d plots to Mattermost...\n", length(temp_files)))

    tryCatch({
      named_temp_files <- temp_files
      names(named_temp_files) <- plot_titles

      base_message <- sprintf("Skymap analysis: %d plots generated", length(temp_files))
      full_message <- if (!is.null(mattermost_message_prefix)) {
        paste0(mattermost_message_prefix, "\n", base_message)
      } else {
        base_message
      }

      send_multiple_plots_mattermost(
        temp_files = named_temp_files,
        mattermost_url = mattermost_config$url,
        token = mattermost_config$token,
        team_name = mattermost_config$team,
        channel_name = mattermost_config$channel,
        message = full_message
      )

      cat("All plots sent to Mattermost\n")
    }, error = function(e) {
      cat(sprintf("Failed to send to Mattermost: %s\n", e$message))
    })

    # Cleanup temp files
    for (tf in temp_files) {
      if (file.exists(tf)) unlink(tf)
    }
    cat("Cleaned up temporary files\n")
  }

  return(invisible(all_plots))
}


#' Brew Skymap Plot Chunks for R Markdown
#'
#' Wrapper function for \code{\link{brew_plot_chunks}} specifically configured
#' for skymap plots from pivoted data. Creates R Markdown chunks that can be
#' included in reports.
#'
#' @param plot_list Named list of ggplot2 objects from \code{\link{plot_all_skymaps_pivoted}}.
#' @param output_file Character. Optional output file path for the brewed chunks.
#'   If NULL, chunks are returned as a character string.
#'
#' @return If output_file is NULL, returns character string of R Markdown chunks.
#'   Otherwise, writes to file and returns the file path invisibly.
#'
#' @details
#' This wrapper sets appropriate figure dimensions (12x8) for skymap plots
#' and uses a consistent file pattern for temporary chunk files.
#'
#' @seealso \code{\link{plot_all_skymaps_pivoted}}, \code{\link{brew_plot_chunks}}
#'
#' @examples
#' \dontrun{
#' # Generate all skymap plots
#' all_plots <- plot_all_skymaps_pivoted(skymap_pivoted, show_plots = FALSE)
#'
#' # Create R Markdown chunks
#' chunks <- brew_skymap_chunks_pivoted(all_plots)
#'
#' # Or write directly to file
#' brew_skymap_chunks_pivoted(all_plots, output_file = "skymap_chunks.Rmd")
#' }
#'
#' @export
brew_skymap_chunks_pivoted <- function(plot_list, output_file = NULL) {
  brew_plot_chunks(
    plot_list = plot_list,
    plot_list_var_name = deparse(substitute(plot_list)),
    output_file = output_file,
    fig_height = 8,
    fig_width = 12,
    file_pattern = file.path(tempdir(), "brewed_skymap_chunks_pivoted_")
  )
}
