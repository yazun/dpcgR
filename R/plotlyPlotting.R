

# Create reverse alias map: alias -> full_type
create_reverse_alias_map <- function(alias_map) {
  # alias_map is: full_type -> alias (e.g., "classifier:type" -> "c1")
  # We need: alias -> full_type
  reverse_map <- setNames(names(alias_map), unlist(alias_map))
  return(reverse_map)
}

# Function to expand aliases in pivoted data to full names
expand_aliases_in_pivoted <- function(skymap_pivoted, alias_map) {
  reverse_map <- create_reverse_alias_map(alias_map)

  # Map labels from aliases to full names
  skymap_pivoted <- skymap_pivoted %>%
    mutate(
      label = ifelse(label %in% names(reverse_map),
                     reverse_map[label],
                     label)
    )

  return(skymap_pivoted)
}

# Wrapper function to plot histograms from skymap results
#' PLotting skymaps from parallel aggregates.
#'
#' @param results Results to plot
#' @param output_dir output_dir
#' @param show_plots if to show plots straight away via print
#' @param show_training if to show training sources in the plots
#'
#' @return set of histograms
#' @export
#'
plot_histograms_from_skymap_results <- function(results,
                                                output_dir = "histogram_plots",
                                                show_plots = TRUE,
                                                show_training = TRUE) {

  # Check if we have the required data
  if (is.null(results$skymap_pivoted)) {
    stop("results$skymap_pivoted is required")
  }

  sosname <- results$metadata$sosname
  cuts_info <- results$metadata$cuts

  # Expand aliases to full names using alias_map
  skymap_data <- results$skymap_pivoted
  if (!is.null(results$skymap$alias_map)) {
    skymap_data <- expand_aliases_in_pivoted(skymap_data, results$skymap$alias_map)
  }

  # Get unique labels (excluding "all_conditions")
  all_labels <- unique(skymap_data$label)
  all_labels <- all_labels[all_labels != "all_conditions"]

  # Check if labels contain ":" separator
  has_separator <- any(grepl(":", all_labels))

  if (!has_separator) {
    warning("Labels do not contain ':' separator. Using labels as-is.")
    classifier_names <- all_labels
  } else {
    # Extract unique classifier names (part before ":")
    classifier_names <- unique(sapply(strsplit(all_labels, ":"), `[`, 1))
  }

  # Store all plots
  all_plots <- list()

  cat(sprintf("\n=== Plotting histograms for %s ===\n", sosname))
  cat(sprintf("  Found %d classifiers: %s\n", length(classifier_names),
              paste(classifier_names, collapse = ", ")))

  # Plot per-classifier histograms
  for (classifier_name in classifier_names) {
    cat(sprintf("  Plotting %s - with cuts\n", classifier_name))

    p1 <- plot_histogram_from_skymap_pivoted(
      skymap_data = skymap_data,
      sosname = sosname,
      cuts_info = cuts_info,
      classifier_name = classifier_name,
      cut_type = "with_cut",
      show_training = show_training,
      output_dir = output_dir
    )

    if (!is.null(p1)) {
      plot_name <- sprintf("%s_%s_with_cut", sosname, classifier_name)
      all_plots[[plot_name]] <- p1
      if (show_plots) print(p1)
    }

    cat(sprintf("  Plotting %s - no cuts\n", classifier_name))

    p2 <- plot_histogram_from_skymap_pivoted(
      skymap_data = skymap_data,
      sosname = sosname,
      cuts_info = cuts_info,
      classifier_name = classifier_name,
      cut_type = "no_cut",
      show_training = show_training,
      output_dir = output_dir
    )

    if (!is.null(p2)) {
      plot_name <- sprintf("%s_%s_no_cut", sosname, classifier_name)
      all_plots[[plot_name]] <- p2
      if (show_plots) print(p2)
    }
  }

  # Plot merged histograms (all_conditions)
  cat(sprintf("  Plotting merged - with cuts\n"))

  p3 <- plot_histogram_from_skymap_pivoted(
    skymap_data = skymap_data,
    sosname = sosname,
    cuts_info = cuts_info,
    classifier_name = NULL,  # NULL means merged/all_conditions
    cut_type = "with_cut",
    show_training = show_training,
    output_dir = output_dir
  )

  if (!is.null(p3)) {
    plot_name <- sprintf("%s_merged_with_cut", sosname)
    all_plots[[plot_name]] <- p3
    if (show_plots) print(p3)
  }

  cat(sprintf("  Plotting merged - no cuts\n"))

  p4 <- plot_histogram_from_skymap_pivoted(
    skymap_data = skymap_data,
    sosname = sosname,
    cuts_info = cuts_info,
    classifier_name = NULL,
    cut_type = "no_cut",
    show_training = show_training,
    output_dir = output_dir
  )

  if (!is.null(p4)) {
    plot_name <- sprintf("%s_merged_no_cut", sosname)
    all_plots[[plot_name]] <- p4
    if (show_plots) print(p4)
  }

  cat(sprintf("\n=== Completed plotting %d histograms ===\n", length(all_plots)))

  return(invisible(all_plots))
}

#' Add Cut Lines to Histogram Plot
#'
#' Adds vertical dashed lines indicating probability cut thresholds to a Plotly
#' histogram. Groups cuts by threshold value - identical thresholds share a line
#' with combined legend, while different thresholds get distinct colors.
#'
#' @param fig A Plotly figure object to add cut lines to.
#' @param cuts_info A nested list of probability cut thresholds, structured as
#'   \code{cuts_info[[classifier_name]][[type_label]]} = cut_value.
#' @param classifier_name Character string specifying which classifier's cuts to show.
#'   If \code{NULL}, shows cuts from all classifiers (for merged plots).
#' @param y_max Numeric. Maximum y-value for the cut lines (typically 110% of
#'   max inverse cumsum).
#' @param is_merged Logical. Whether this is a merged plot (affects which cuts to show).
#'
#' @return The modified Plotly figure object with cut lines added.
#'
#' @details
#' Cut lines are grouped by their threshold value:
#' \itemize{
#'   \item Cuts with identical thresholds share a single line with a combined
#'     legend entry listing all types (e.g., "Cut @0.30: CEP, RR, cepIn")
#'   \item Cuts with different thresholds get distinct colors from a predefined
#'     palette and separate legend entries
#' }
#'
#' Color palette cycles through: red, purple, orange, teal, magenta, brown,
#' pink, olive, cyan.
#'
#' @keywords internal
add_cut_lines_to_plot <- function(fig, cuts_info, classifier_name, y_max, is_merged) {

  if (is.null(cuts_info)) {
    return(fig)
  }

  # Collect all relevant cuts with their metadata
  cuts_data <- list()

  if (is_merged) {
    # Collect cuts from all classifiers
    for (clf_name in names(cuts_info)) {
      clf_cuts <- cuts_info[[clf_name]]
      for (type_label in names(clf_cuts)) {
        cut_val <- clf_cuts[[type_label]]
        type_name <- if (grepl(":", type_label)) sub("^[^:]+:", "", type_label) else type_label

        cuts_data[[length(cuts_data) + 1]] <- list(
          threshold = cut_val,
          type_name = type_name,
          classifier = clf_name,
          full_label = type_label
        )
      }
    }
  } else if (!is.null(cuts_info[[classifier_name]])) {
    # Collect cuts for this classifier only
    clf_cuts <- cuts_info[[classifier_name]]
    for (type_label in names(clf_cuts)) {
      cut_val <- clf_cuts[[type_label]]
      type_name <- if (grepl(":", type_label)) sub("^[^:]+:", "", type_label) else type_label

      cuts_data[[length(cuts_data) + 1]] <- list(
        threshold = cut_val,
        type_name = type_name,
        classifier = classifier_name,
        full_label = type_label
      )
    }
  }

  if (length(cuts_data) == 0) {
    return(fig)
  }

  # Group cuts by threshold value
  cuts_by_threshold <- list()
  for (cut_info in cuts_data) {
    threshold_key <- sprintf("%.6f", cut_info$threshold)  # Use string key for grouping
    if (is.null(cuts_by_threshold[[threshold_key]])) {
      cuts_by_threshold[[threshold_key]] <- list(
        threshold = cut_info$threshold,
        types = c()
      )
    }
    cuts_by_threshold[[threshold_key]]$types <- c(
      cuts_by_threshold[[threshold_key]]$types,
      cut_info$type_name
    )
  }

  # Define color palette for different thresholds
  cut_colors <- c(
    '#e41a1c',  # red
    '#984ea3',  # purple
    '#ff7f00',  # orange
    '#17becf',  # teal/cyan
    '#e377c2',  # magenta/pink
    '#8c564b',  # brown
    '#f781bf',  # light pink
    '#999999',  # gray
    '#a65628',  # dark orange/brown
    '#377eb8'   # blue
  )

  # Get unique thresholds sorted
  unique_thresholds <- sort(unique(sapply(cuts_by_threshold, function(x) x$threshold)))
  n_unique <- length(unique_thresholds)

  # Add cut lines
  for (i in seq_along(unique_thresholds)) {
    threshold <- unique_thresholds[i]
    threshold_key <- sprintf("%.6f", threshold)
    cut_group <- cuts_by_threshold[[threshold_key]]

    # Get unique type names and sort them
    type_names <- unique(cut_group$types)
    type_names <- sort(type_names)

    # Build legend label
    if (length(type_names) <= 3) {
      types_str <- paste(type_names, collapse = ", ")
    } else {
      # Abbreviate if too many types
      types_str <- paste0(paste(type_names[1:3], collapse = ", "), ", +", length(type_names) - 3, " more")
    }

    legend_label <- sprintf("Cut @%.2f: %s", threshold, types_str)

    # Select color - use single color if all same threshold, otherwise cycle through palette
    if (n_unique == 1) {
      line_color <- '#e41a1c'  # Default red for single threshold
    } else {
      line_color <- cut_colors[((i - 1) %% length(cut_colors)) + 1]
    }

    # Add the cut line
    fig <- fig %>%
      plotly::add_trace(
        x = rep(threshold, 2),
        y = c(0, y_max),
        type = 'scatter',
        mode = 'lines',
        name = legend_label,
        line = list(color = line_color, width = 2, dash = 'dash'),
        yaxis = 'y2',
        showlegend = TRUE,
        hoverinfo = 'text',
        text = sprintf("Threshold: %.3f<br>Types: %s", threshold, paste(type_names, collapse = ", "))
      )
  }

  return(fig)
}


#' Plot Histogram from Pivoted Skymap Data
#'
#' Creates an interactive Plotly histogram showing the distribution of posterior
#' probabilities for astronomical source classifications. The histogram is constructed
#' by binning average probability values and summing source counts per bin.
#'
#' @param skymap_data A dataframe containing pivoted skymap data with columns:
#'   \describe{
#'     \item{label}{Character. Classifier:type identifier or "all_conditions"}
#'     \item{cut_type}{Character. Either "with_cut" or "no_cut"}
#'     \item{count}{Numeric. Number of sources in each HEALPix pixel}
#'     \item{avg_prob}{Numeric. Average posterior probability in each pixel}
#'     \item{training_count}{Numeric. Number of training sources in each pixel}
#'   }
#' @param sosname Character string identifying the SOS (Specific Object Study) name.
#' @param cuts_info A nested list of probability cut thresholds, structured as
#'   \code{cuts_info[[classifier_name]][[type_label]]} = cut_value.
#' @param classifier_name Character string specifying which classifier to plot.
#'   If \code{NULL}, creates a merged plot using "all_conditions" data.
#' @param cut_type Character string, either \code{"with_cut"} or \code{"no_cut"},
#'   specifying which subset of the data to plot.
#' @param show_cumsum Logical indicating whether to display inverse cumulative sum
#'   lines on a secondary y-axis. Default is \code{TRUE}.
#' @param show_cuts Logical indicating whether to display vertical lines at
#'   probability cut thresholds. Default is \code{TRUE}.
#' @param show_training Logical indicating whether to include training source
#'   counts as separate bar series. Default is \code{TRUE}.
#' @param output_dir Character string specifying directory to save the plot as
#'   an HTML file. If \code{NULL} (default), the plot is not saved.
#'
#' @return A Plotly figure object, or \code{NULL} if no data is available for
#'   the specified parameters.
#'
#' @details
#' The function performs the following processing:
#' \enumerate{
#'   \item Filters data by \code{cut_type} and \code{classifier_name}
#'   \item Bins probability values into 20 equal-width bins from 0 to 1
#'   \item Aggregates source counts per bin
#'   \item Calculates inverse cumulative sums (sources with probability >= bin threshold)
#'   \item Automatically applies log scale if frequency range exceeds 5x
#'   \item Adds subtitle showing accumulated source counts from probability cut per class
#'   \item Color-codes cut lines by threshold value, grouping identical thresholds
#' }
#'
#' The resulting plot includes:
#' \itemize{
#'   \item Primary y-axis: Bar chart of source frequencies per probability bin
#'   \item Secondary y-axis: Inverse cumulative sum line(s)
#'   \item Vertical dashed lines at probability cut thresholds (color-coded by value)
#'   \item Green bars/lines for training source comparison (if \code{show_training = TRUE})
#'   \item Subtitle with accumulated entries from cut threshold per class
#' }
#'
#' @section Cut Line Colors:
#' Cut lines are color-coded by threshold value:
#' \itemize{
#'   \item Cuts with identical thresholds share a single line with combined legend
#'   \item Cuts with different thresholds get distinct colors
#'   \item Legend shows threshold value and associated types
#' }
#'
#' @seealso
#' \code{\link{plot_histograms_from_skymap_results}} for batch plotting,
#' \code{\link{add_cut_lines_to_plot}} for cut line implementation
#'
#' @examples
#' \dontrun{
#' # Plot merged histogram with color-coded cuts
#' fig <- plot_histogram_from_skymap_pivoted(
#'   skymap_data = expanded_skymap_data,
#'   sosname = "cepheidandrrlyrae",
#'   cuts_info = results$metadata$cuts,
#'   classifier_name = NULL,
#'   cut_type = "with_cut",
#'   show_training = TRUE
#' )
#' print(fig)
#' }
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr filter mutate group_by summarise arrange ungroup n_distinct
#' @importFrom htmlwidgets saveWidget
#' @export
plot_histogram_from_skymap_pivoted <- function(skymap_data,
                                               sosname,
                                               cuts_info,
                                               classifier_name = NULL,
                                               cut_type = "with_cut",
                                               show_cumsum = TRUE,
                                               show_cuts = TRUE,
                                               show_training = TRUE,
                                               output_dir = NULL) {

  is_merged <- is.null(classifier_name)

  # Filter by cut_type
  data <- skymap_data %>%
    dplyr::filter(cut_type == !!cut_type)

  if (nrow(data) == 0) {
    warning(sprintf("No data for cut_type: %s", cut_type))
    return(NULL)
  }

  # Filter by classifier
  if (is_merged) {
    # For merged plot, aggregate across ALL types (not "all_conditions" which has NA avg_prob)
    data <- data %>% dplyr::filter(label != "all_conditions")

    if (nrow(data) == 0) {
      warning("No individual type data available for merged plot")
      return(NULL)
    }

    title <- sprintf("%s - Merged (%s)", sosname,
                     ifelse(cut_type == "with_cut", "With Cuts", "No Cuts"))
  } else {
    # Get all types for this classifier
    all_labels <- unique(data$label)
    all_labels <- all_labels[all_labels != "all_conditions"]
    has_separator <- any(grepl(":", all_labels))

    if (has_separator) {
      classifier_pattern <- paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", classifier_name), ":")
      data <- data %>% dplyr::filter(grepl(classifier_pattern, label))
    } else {
      data <- data %>% dplyr::filter(label == classifier_name)
    }

    title <- sprintf("%s - %s (%s)", sosname, classifier_name,
                     ifelse(cut_type == "with_cut", "With Cuts", "No Cuts"))
  }

  if (nrow(data) == 0) {
    warning(sprintf("No data available for: %s", title))
    return(NULL)
  }

  # Remove NA values for avg_prob
  data <- data %>% dplyr::filter(!is.na(avg_prob))

  if (nrow(data) == 0) {
    warning(sprintf("No valid avg_prob data for: %s", title))
    return(NULL)
  }

  # Bin the data by avg_prob (0-1 range, 20 bins)
  n_bins <- 20
  breaks <- seq(0, 1, length.out = n_bins + 1)

  # Aggregate counts per bin
  if (is_merged) {
    binned_data <- data %>%
      dplyr::mutate(bin = cut(avg_prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)) %>%
      dplyr::filter(!is.na(bin)) %>%
      dplyr::group_by(bin) %>%
      dplyr::summarise(
        freq = sum(count, na.rm = TRUE),
        training_freq = sum(training_count, na.rm = TRUE),
        n_types = dplyr::n_distinct(label),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        bucket_start = breaks[bin],
        bucket_end = breaks[bin + 1],
        bucket_mid = (bucket_start + bucket_end) / 2
      ) %>%
      dplyr::arrange(bin)
  } else {
    binned_data <- data %>%
      dplyr::mutate(bin = cut(avg_prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)) %>%
      dplyr::filter(!is.na(bin)) %>%
      dplyr::group_by(label, bin) %>%
      dplyr::summarise(
        freq = sum(count, na.rm = TRUE),
        training_freq = sum(training_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        bucket_start = breaks[bin],
        bucket_end = breaks[bin + 1],
        bucket_mid = (bucket_start + bucket_end) / 2
      ) %>%
      dplyr::arrange(label, bin)
  }

  if (nrow(binned_data) == 0) {
    warning(sprintf("No binned data for: %s", title))
    return(NULL)
  }

  # Add inverse cumulative sum
  if (is_merged) {
    binned_data <- binned_data %>%
      dplyr::arrange(bin) %>%
      dplyr::mutate(
        forward_cumsum = cumsum(freq),
        total_freq = sum(freq),
        inverse_cumsum = total_freq - forward_cumsum + freq,
        inverse_cumsum_norm = inverse_cumsum / total_freq,
        training_forward_cumsum = cumsum(training_freq),
        training_total_freq = sum(training_freq),
        training_inverse_cumsum = training_total_freq - training_forward_cumsum + training_freq,
        training_inverse_cumsum_norm = ifelse(training_total_freq > 0,
                                              training_inverse_cumsum / training_total_freq, 0)
      )
  } else {
    binned_data <- binned_data %>%
      dplyr::group_by(label) %>%
      dplyr::arrange(bin) %>%
      dplyr::mutate(
        forward_cumsum = cumsum(freq),
        total_freq = sum(freq),
        inverse_cumsum = total_freq - forward_cumsum + freq,
        inverse_cumsum_norm = inverse_cumsum / total_freq,
        training_forward_cumsum = cumsum(training_freq),
        training_total_freq = sum(training_freq),
        training_inverse_cumsum = training_total_freq - training_forward_cumsum + training_freq,
        training_inverse_cumsum_norm = ifelse(training_total_freq > 0,
                                              training_inverse_cumsum / training_total_freq, 0)
      ) %>%
      dplyr::ungroup()
  }

  # ============================================================================
  # Calculate accumulated entries from probability cut (for subtitle)
  # ============================================================================
  subtitle_parts <- c()

  if (!is.null(cuts_info)) {
    if (is_merged) {
      # For merged plot: calculate for all classifiers/types
      for (clf_name in names(cuts_info)) {
        clf_cuts <- cuts_info[[clf_name]]
        for (type_label in names(clf_cuts)) {
          cut_val <- clf_cuts[[type_label]]
          type_name <- if (grepl(":", type_label)) sub("^[^:]+:", "", type_label) else type_label

          accum_count <- binned_data %>%
            dplyr::filter(bucket_start >= cut_val) %>%
            dplyr::summarise(total = sum(freq, na.rm = TRUE)) %>%
            dplyr::pull(total)

          accum_training <- binned_data %>%
            dplyr::filter(bucket_start >= cut_val) %>%
            dplyr::summarise(total = sum(training_freq, na.rm = TRUE)) %>%
            dplyr::pull(total)

          if (length(accum_count) > 0 && !is.na(accum_count) && accum_count > 0) {
            subtitle_parts <- c(subtitle_parts,
                                sprintf("%s: %s (tr: %s) @>=%.2f",
                                        type_name,
                                        format(accum_count, big.mark = ","),
                                        format(accum_training, big.mark = ","),
                                        cut_val))
          }
        }
      }
    } else if (!is.null(cuts_info[[classifier_name]])) {
      clf_cuts <- cuts_info[[classifier_name]]

      for (type_label in names(clf_cuts)) {
        cut_val <- clf_cuts[[type_label]]
        type_name <- if (grepl(":", type_label)) sub("^[^:]+:", "", type_label) else type_label

        type_data <- binned_data %>%
          dplyr::filter(grepl(paste0(":", type_name, "$"), label) | label == type_name)

        if (nrow(type_data) > 0) {
          accum_count <- type_data %>%
            dplyr::filter(bucket_start >= cut_val) %>%
            dplyr::summarise(total = sum(freq, na.rm = TRUE)) %>%
            dplyr::pull(total)

          accum_training <- type_data %>%
            dplyr::filter(bucket_start >= cut_val) %>%
            dplyr::summarise(total = sum(training_freq, na.rm = TRUE)) %>%
            dplyr::pull(total)
        } else {
          accum_count <- binned_data %>%
            dplyr::filter(bucket_start >= cut_val) %>%
            dplyr::summarise(total = sum(freq, na.rm = TRUE)) %>%
            dplyr::pull(total)

          accum_training <- binned_data %>%
            dplyr::filter(bucket_start >= cut_val) %>%
            dplyr::summarise(total = sum(training_freq, na.rm = TRUE)) %>%
            dplyr::pull(total)
        }

        if (length(accum_count) > 0 && !is.na(accum_count) && accum_count > 0) {
          subtitle_parts <- c(subtitle_parts,
                              sprintf("%s: %s (tr: %s) @>=%.2f",
                                      type_name,
                                      format(accum_count, big.mark = ","),
                                      format(accum_training, big.mark = ","),
                                      cut_val))
        }
      }
    }
  }

  # Build subtitle string
  subtitle <- if (length(subtitle_parts) > 0) {
    paste("Sources from cut:", paste(subtitle_parts, collapse = " | "))
  } else {
    NULL
  }

  # Determine if log scale needed
  positive_freqs <- binned_data$freq[binned_data$freq > 0]
  use_log <- FALSE
  if (length(positive_freqs) >= 2) {
    freq_range <- range(positive_freqs)
    use_log <- (freq_range[2] / freq_range[1]) > 5
  }

  # Create hover text
  if (is_merged) {
    binned_data <- binned_data %>%
      dplyr::mutate(
        hover_text = paste0(
          "Probability: ", round(bucket_mid, 3), "<br>",
          "Frequency: ", format(freq, big.mark = ","), "<br>",
          "Training: ", format(training_freq, big.mark = ","), "<br>",
          "Types: ", n_types, "<br>",
          "Inv. Cumsum: ", format(inverse_cumsum, big.mark = ","), "<br>",
          "Inv. Cumsum %: ", round(inverse_cumsum_norm * 100, 2), "%"
        ),
        hover_text_training = paste0(
          "Probability: ", round(bucket_mid, 3), "<br>",
          "Training Sources: ", format(training_freq, big.mark = ","), "<br>",
          "Training Inv. Cumsum: ", format(training_inverse_cumsum, big.mark = ",")
        )
      )
  } else {
    binned_data <- binned_data %>%
      dplyr::mutate(
        type_name = ifelse(grepl(":", label), sub("^[^:]+:", "", label), label),
        hover_text = paste0(
          "Type: ", type_name, "<br>",
          "Probability: ", round(bucket_mid, 3), "<br>",
          "Frequency: ", format(freq, big.mark = ","), "<br>",
          "Training: ", format(training_freq, big.mark = ","), "<br>",
          "Inv. Cumsum: ", format(inverse_cumsum, big.mark = ","), "<br>",
          "Inv. Cumsum %: ", round(inverse_cumsum_norm * 100, 2), "%"
        ),
        hover_text_training = paste0(
          "Type: ", type_name, "<br>",
          "Probability: ", round(bucket_mid, 3), "<br>",
          "Training Sources: ", format(training_freq, big.mark = ","), "<br>",
          "Training Inv. Cumsum: ", format(training_inverse_cumsum, big.mark = ",")
        )
      )
  }

  # Create plot
  fig <- plotly::plot_ly()

  if (is_merged) {
    # Merged plot - single bar series
    fig <- fig %>%
      plotly::add_trace(
        data = binned_data %>% dplyr::filter(freq > 0),
        x = ~bucket_mid,
        y = ~freq,
        type = 'bar',
        name = "All Types (Merged)",
        text = ~hover_text,
        hoverinfo = 'text',
        marker = list(color = 'steelblue')
      )

    if (show_training && any(binned_data$training_freq > 0)) {
      fig <- fig %>%
        plotly::add_trace(
          data = binned_data %>% dplyr::filter(training_freq > 0),
          x = ~bucket_mid,
          y = ~training_freq,
          type = 'bar',
          name = "Training Sources",
          text = ~hover_text_training,
          hoverinfo = 'text',
          marker = list(color = 'green', opacity = 0.7)
        )
    }

    if (show_cumsum) {
      fig <- fig %>%
        plotly::add_trace(
          data = binned_data,
          x = ~bucket_mid,
          y = ~inverse_cumsum,
          type = 'scatter',
          mode = 'lines',
          name = "Inv. Cumsum",
          line = list(width = 2, color = 'red'),
          yaxis = 'y2'
        )

      if (show_training && any(binned_data$training_freq > 0)) {
        fig <- fig %>%
          plotly::add_trace(
            data = binned_data %>% dplyr::filter(training_freq > 0),
            x = ~bucket_mid,
            y = ~training_inverse_cumsum,
            type = 'scatter',
            mode = 'lines',
            name = "Training Inv. Cumsum",
            line = list(width = 2, color = 'darkgreen', dash = 'dot'),
            yaxis = 'y2'
          )
      }
    }
  } else {
    # Per-classifier plot - bars by type
    unique_labels <- unique(binned_data$label)
    colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
                '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')

    for (i in seq_along(unique_labels)) {
      lbl <- unique_labels[i]
      data_subset <- binned_data %>% dplyr::filter(label == lbl)
      type_display <- if (grepl(":", lbl)) sub("^[^:]+:", "", lbl) else lbl

      fig <- fig %>%
        plotly::add_trace(
          data = data_subset %>% dplyr::filter(freq > 0),
          x = ~bucket_mid,
          y = ~freq,
          type = 'bar',
          name = type_display,
          text = ~hover_text,
          hoverinfo = 'text',
          legendgroup = lbl,
          marker = list(color = colors[((i-1) %% length(colors)) + 1])
        )

      if (show_training && any(data_subset$training_freq > 0)) {
        fig <- fig %>%
          plotly::add_trace(
            data = data_subset %>% dplyr::filter(training_freq > 0),
            x = ~bucket_mid,
            y = ~training_freq,
            type = 'bar',
            name = paste0(type_display, " (Training)"),
            text = ~hover_text_training,
            hoverinfo = 'text',
            legendgroup = lbl,
            marker = list(color = 'green', opacity = 0.5)
          )
      }

      if (show_cumsum) {
        fig <- fig %>%
          plotly::add_trace(
            data = data_subset,
            x = ~bucket_mid,
            y = ~inverse_cumsum,
            type = 'scatter',
            mode = 'lines+markers',
            name = paste0(type_display, " (Cumsum)"),
            line = list(width = 2, color = colors[((i-1) %% length(colors)) + 1]),
            legendgroup = lbl,
            yaxis = 'y2'
          )

        if (show_training && any(data_subset$training_freq > 0)) {
          fig <- fig %>%
            plotly::add_trace(
              data = data_subset %>% dplyr::filter(training_freq > 0),
              x = ~bucket_mid,
              y = ~training_inverse_cumsum,
              type = 'scatter',
              mode = 'lines',
              name = paste0(type_display, " (Training Cumsum)"),
              line = list(width = 2, color = 'darkgreen', dash = 'dot'),
              legendgroup = lbl,
              yaxis = 'y2'
            )
        }
      }
    }
  }

  # Add color-coded cut lines
  if (show_cuts && !is.null(cuts_info)) {
    y_max <- max(binned_data$inverse_cumsum, na.rm = TRUE) * 1.1
    fig <- add_cut_lines_to_plot(fig, cuts_info, classifier_name, y_max, is_merged)
  }

  # ============================================================================
  # Layout with title and subtitle
  # ============================================================================
  title_config <- if (!is.null(subtitle)) {
    list(
      text = paste0(title, "<br><sup>", subtitle, "</sup>"),
      font = list(size = 14),
      x = 0.5,
      xanchor = "center"
    )
  } else {
    list(
      text = title,
      font = list(size = 14),
      x = 0.5,
      xanchor = "center"
    )
  }

  fig <- fig %>%
    plotly::layout(
      title = title_config,
      xaxis = list(title = "Posterior Probability"),
      yaxis = list(
        title = if (use_log) "Frequency (log)" else "Frequency",
        type = if (use_log) "log" else "linear"
      ),
      yaxis2 = if (show_cumsum) list(
        title = "Inverse Cumsum",
        overlaying = 'y',
        side = 'right',
        type = if (use_log) "log" else "linear"
      ) else NULL,
      barmode = 'group',
      hovermode = 'closest',
      legend = list(
        orientation = 'v',
        x = 1.15,
        y = 1
      ),
      margin = list(t = 80)
    )

  # Save if requested
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    filename <- if (is_merged) {
      sprintf("%s_merged_%s.html", sosname, cut_type)
    } else {
      sprintf("%s_%s_%s.html", sosname, classifier_name, cut_type)
    }

    htmlwidgets::saveWidget(fig, file.path(output_dir, filename), selfcontained = TRUE)
  }

  return(fig)
}
