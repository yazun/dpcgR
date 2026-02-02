#' Create Reverse Alias Map
#'
#' Converts an alias map from full_type -> alias format to alias -> full_type format.
#' This is useful for mapping short column aliases back to their full classifier:type names.
#'
#' @param alias_map A named list or vector where names are full type identifiers
#'   (e.g., "classifier_name:type_name") and values are short aliases (e.g., "c1", "c2").
#'
#' @return A named character vector where names are aliases and values are full type identifiers.
#'
#' @examples {
#'
#' alias_map <- list(
#'   "CD_DR4_xgboost:CEP" = "c1",
#'   "CD_DR4_xgboost:RR" = "c2"
#' )
#' reverse_map <- create_reverse_alias_map(alias_map)
#' # Returns: c(c1 = "CD_DR4_xgboost:CEP", c2 = "CD_DR4_xgboost:RR")
#' }
#' @export
create_reverse_alias_map <- function(alias_map) {

  reverse_map <- setNames(names(alias_map), unlist(alias_map))
  return(reverse_map)
}


#' Expand Aliases in Pivoted Skymap Data
#'
#' Replaces alias labels (e.g., "c1", "c2") in a pivoted skymap dataframe with
#' their full classifier:type names using the provided alias map.
#'
#' @param skymap_pivoted A dataframe containing pivoted skymap data with a \code{label}
#'   column that may contain alias values.
#' @param alias_map A named list or vector mapping full type identifiers to aliases,
#'   as returned by skymap query functions.
#'
#' @return A dataframe with the same structure as \code{skymap_pivoted}, but with
#'   alias values in the \code{label} column replaced by full type identifiers.
#'
#' @details
#' The function uses \code{\link{create_reverse_alias_map}} internally to convert
#' the alias map to the required format. Labels that do not match any alias
#' (e.g., "all_conditions") are left unchanged.
#'
#' @examples {
#' \dontrun{
#' # Assuming results from skymap query
#' expanded_data <- expand_aliases_in_pivoted(
#'   results$skymap_pivoted,
#'   results$skymap$alias_map
#' )
#' }
#' }
#' @importFrom dplyr mutate
#' @export
expand_aliases_in_pivoted <- function(skymap_pivoted, alias_map) {
  reverse_map <- create_reverse_alias_map(alias_map)


  skymap_pivoted <- skymap_pivoted %>%
    dplyr::mutate(
      label = ifelse(label %in% names(reverse_map),
                     reverse_map[label],
                     label)
    )

  return(skymap_pivoted)
}


#' Plot Histograms from Skymap Analysis Results
#'
#' Generates interactive Plotly histogram plots from skymap analysis results,
#' showing the distribution of posterior probabilities for astronomical source
#' classifications. Creates separate plots for each classifier and merged plots
#' combining all classifiers.
#'
#' @param results A list containing skymap analysis results with the following components:
#'   \describe{
#'     \item{skymap_pivoted}{Dataframe with columns: label, cut_type, coord, alpha, delta,
#'       count, avg_prob, min_prob, max_prob, training_count}
#'     \item{skymap}{List containing alias_map for label expansion}
#'     \item{metadata}{List containing sosname (SOS name) and cuts (probability thresholds)}
#'   }
#' @param output_dir Character string specifying the directory to save HTML plot files.
#'   If \code{NULL} (default), plots are not saved to files.
#' @param show_plots Logical indicating whether to display plots interactively.
#'   Default is \code{TRUE}.
#' @param show_training Logical indicating whether to include training source counts
#'   as separate bar series. Default is \code{TRUE}.
#'
#' @return Invisibly returns a named list of Plotly figure objects. Names follow the
#'   pattern \code{{sosname}_{classifier}_{cut_type}} for per-classifier plots and
#'   \code{{sosname}_merged_{cut_type}} for merged plots.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Expands alias labels to full classifier:type names using the alias map
#'   \item Identifies unique classifiers from the data
#'   \item Generates four types of plots for each classifier:
#'     \itemize{
#'       \item Per-classifier with probability cuts applied
#'       \item Per-classifier without cuts
#'       \item Merged (all classifiers) with cuts
#'       \item Merged without cuts
#'     }
#' }
#'
#' Each plot includes:
#' \itemize{
#'   \item Bar chart of source counts per probability bin
#'   \item Optional training source counts (green bars)
#'   \item Inverse cumulative sum line (secondary y-axis)
#'   \item Vertical lines indicating probability cut thresholds
#' }
#'
#' @seealso
#' \code{\link{plot_histogram_from_skymap_pivoted}} for the underlying plotting function,
#' \code{\link{expand_aliases_in_pivoted}} for alias expansion
#'
#' @examples{
#' \dontrun{
#' # After running skymap analysis
#' results <- run_skymap_analysis(params, runid, sosname_conf)
#'
#' # Generate all plots with training comparison
#' all_plots <- plot_histograms_from_skymap_results(
#'   results,
#'   output_dir = "histogram_plots",
#'   show_plots = TRUE,
#'   show_training = TRUE
#' )
#'
#' # Access individual plots
#' all_plots$cepheidandrrlyrae_CD_DR4_xgboost_with_cut
#' }
#' }
#'
#' @importFrom dplyr filter mutate
#' @export
plot_histograms_from_skymap_results <- function(results,
                                                output_dir = NULL,
                                                show_plots = TRUE,
                                                show_training = TRUE) {

  # Validate input

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
  cat("  Plotting merged - with cuts\n")

  p3 <- plot_histogram_from_skymap_pivoted(
    skymap_data = skymap_data,
    sosname = sosname,
    cuts_info = cuts_info,
    classifier_name = NULL,
    cut_type = "with_cut",
    show_training = show_training,
    output_dir = output_dir
  )

  if (!is.null(p3)) {
    plot_name <- sprintf("%s_merged_with_cut", sosname)
    all_plots[[plot_name]] <- p3
    if (show_plots) print(p3)
  }

  cat("  Plotting merged - no cuts\n")

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
#' }
#'
#' The resulting plot includes:
#' \itemize{
#'   \item Primary y-axis: Bar chart of source frequencies per probability bin
#'   \item Secondary y-axis: Inverse cumulative sum line(s)
#'   \item Vertical dashed lines at probability cut thresholds (if \code{show_cuts = TRUE})
#'   \item Green bars/lines for training source comparison (if \code{show_training = TRUE})
#' }
#'
#' @section Hover Information:
#' Each bar displays hover text including:
#' \itemize{
#'   \item Probability bin midpoint
#'   \item Source frequency count
#'   \item Training source count
#'   \item Inverse cumulative sum and percentage
#' }
#'
#' @seealso \code{\link{plot_histograms_from_skymap_results}} for batch plotting
#'
#' @examples
#' \dontrun{
#' # Plot merged histogram with cuts
#' fig <- plot_histogram_from_skymap_pivoted(
#'   skymap_data = expanded_skymap_data,
#'   sosname = "cepheidandrrlyrae",
#'   cuts_info = results$metadata$cuts,
#'   classifier_name = NULL,
#'   cut_type = "with_cut",
#'   show_training = TRUE
#' )
#' print(fig)
#'
#' # Plot specific classifier without cuts
#' fig2 <- plot_histogram_from_skymap_pivoted(
#'   skymap_data = expanded_skymap_data,
#'   sosname = "cepheidandrrlyrae",
#'   cuts_info = results$metadata$cuts,
#'   classifier_name = "CD_DR4_xgboost_multiclass_251120",
#'   cut_type = "no_cut",
#'   show_training = FALSE,
#'   output_dir = "plots"
#' )
#' }
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr filter mutate group_by summarise arrange ungroup
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
    if ("all_conditions" %in% unique(data$label)) {
      data <- data %>% dplyr::filter(label == "all_conditions")
    }
    title <- sprintf("%s - Merged (%s)", sosname,
                     ifelse(cut_type == "with_cut", "With Cuts", "No Cuts"))
  } else {
    all_labels <- unique(data$label)
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

  # Bin the data by avg_prob
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
        name = "All Types",
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

  # Add cut lines if available
  if (show_cuts && !is.null(cuts_info)) {
    y_max <- max(binned_data$inverse_cumsum, na.rm = TRUE) * 1.1

    if (is_merged) {
      for (clf_name in names(cuts_info)) {
        clf_cuts <- cuts_info[[clf_name]]
        for (type_label in names(clf_cuts)) {
          cut_val <- clf_cuts[[type_label]]
          type_name <- if (grepl(":", type_label)) sub("^[^:]+:", "", type_label) else type_label

          fig <- fig %>%
            plotly::add_trace(
              x = rep(cut_val, 2),
              y = c(0, y_max),
              type = 'scatter',
              mode = 'lines',
              name = paste0("Cut: ", type_name),
              line = list(color = 'red', width = 2, dash = 'dash'),
              yaxis = 'y2',
              showlegend = TRUE
            )
        }
      }
    } else if (!is.null(cuts_info[[classifier_name]])) {
      clf_cuts <- cuts_info[[classifier_name]]
      for (type_label in names(clf_cuts)) {
        cut_val <- clf_cuts[[type_label]]
        type_name <- if (grepl(":", type_label)) sub("^[^:]+:", "", type_label) else type_label

        fig <- fig %>%
          plotly::add_trace(
            x = rep(cut_val, 2),
            y = c(0, y_max),
            type = 'scatter',
            mode = 'lines',
            name = paste0("Cut: ", type_name),
            line = list(color = 'red', width = 2, dash = 'dash'),
            yaxis = 'y2',
            showlegend = TRUE
          )
      }
    }
  }

  # Layout
  fig <- fig %>%
    plotly::layout(
      title = title,
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
      )
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
