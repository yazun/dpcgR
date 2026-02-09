#' @title Brew Plot Chunks Utilities
#' @description Functions for generating R Markdown code chunks from named plot lists.
#'   These utilities automate the creation of reproducible report sections from
#'   collections of plots, supporting both static (ggplot2) and interactive (plotly)
#'   visualizations.
#' @name brew-chunks
#' @keywords internal
NULL

library(brew)
library(tools)



#' Generate R Markdown Code Chunks from a Named Plot List
#'
#' Creates an R Markdown file containing code chunks that render each plot from
#' a named list. This function automates report generation by converting plot
#' collections into properly formatted Rmd sections with headers derived from
#' plot names.
#'
#' @param plot_list A named list of plot objects (ggplot2, base R, or any
#'   printable plot object). Names should follow the convention
#'   \code{"sosname_classifier_type"} or \code{"sosname_merged_type"} for
#'   automatic section title generation.
#' @param plot_list_var_name Character string specifying the variable name
#'   that will reference the plot list in the generated chunks. This must
#'   match the actual variable name in the R environment where the Rmd will
#'   be knitted.
#' @param output_file Character string specifying the output file path.
#'   If \code{NULL} (default), creates a temporary file in the system temp
#'   directory.
#' @param fig_height Numeric specifying the figure height in inches for
#'   the generated chunks. Default is \code{10}.
#' @param fig_width Numeric specifying the figure width in inches for
#'   the generated chunks. Default is \code{14}.
#' @param file_pattern Character string specifying the prefix for temporary
#'   file names when \code{output_file} is \code{NULL}. Default is
#'   \code{"brewed_plot_chunks_"}.
#'
#' @return Character string containing the path to the generated R Markdown
#'   file, or \code{NULL} if the plot list is empty.
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Creates or overwrites the output file
#'   \item Parses each plot name to generate readable section headers
#'   \item Creates properly formatted R Markdown code chunks with figure dimensions
#'   \item Writes all chunks sequentially to the output file
#' }
#'
#' \strong{Plot Name Parsing:}
#' Plot names are split by underscore and interpreted as follows:
#' \itemize{
#'   \item Names containing "merged": \code{"SOSNAME_merged_with_cut"} becomes
#'     section header "SOSNAME - Merged (With Cut)"
#'   \item Other names: \code{"SOSNAME_CLASSIFIER_with_cut"} becomes
#'     "SOSNAME - CLASSIFIER (With Cut)"
#' }
#'
#' \strong{Chunk Naming:}
#' Chunk names are sanitized by replacing non-alphanumeric characters with
#' underscores and prefixing with "plot_" to ensure valid R Markdown chunk labels.
#'
#' \strong{Usage in R Markdown:}
#' The generated file is designed to be included in a parent R Markdown document
#' using \code{child} chunks or concatenated with other Rmd content.
#'
#' @examples
#' \dontrun{
#' # Create a named list of plots
#' my_plots <- list(
#'   "RR_LYRAE_DSCTF_with_cut" = ggplot(...),
#'   "RR_LYRAE_merged_no_cut" = ggplot(...)
#' )
#'
#' # Generate Rmd chunks file
#' chunk_file <- brew_plot_chunks(
#'   plot_list = my_plots,
#'   plot_list_var_name = "my_plots",
#'   output_file = "plot_sections.Rmd",
#'   fig_height = 8,
#'   fig_width = 12
#' )
#'
#' # The generated file can be included in main.Rmd:
#' # ```{r child='plot_sections.Rmd'}
#' # ```
#' }
#'
#' @seealso \code{\link{brew_histogram_chunks}} for histogram-specific defaults,
#'   \code{\link{brew_skymap_chunks}} for skymap-specific defaults,
#'   \code{\link{brew_plotly_chunks}} for interactive plotly plots
#'
#' @importFrom tools toTitleCase
#'
#' @export
brew_plot_chunks <- function(plot_list,
                             plot_list_var_name,
                             output_file = NULL,
                             fig_height = 10,
                             fig_width = 14,
                             file_pattern = "brewed_plot_chunks_") {

  # Create temp file if no output specified
  # Uses system temp directory to avoid permission issues

  if (is.null(output_file)) {
    output_file <- tempfile(
      tmpdir = tempdir(),
      pattern = file_pattern,
      fileext = ".Rmd"
    )
  }

  # Clear file if it exists to ensure clean output
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # Extract plot names for iteration
  plot_names <- names(plot_list)

  # Early return with warning if no plots to process
  if (length(plot_names) == 0) {
    warning("No plots found in the list")
    return(NULL)
  }

  # Store backtick character for building code fence syntax
  backtick <- "`"

  # Iterate through each plot and generate corresponding Rmd chunk
  for (i in seq_along(plot_names)) {
    plot_name <- plot_names[i]

    # Parse plot name to create readable section title
    parts <- strsplit(plot_name, "_")[[1]]

    # Create section title based on plot naming convention
    if (length(parts) >= 3 && "merged" %in% parts) {
      # Handle merged plots: "sosname_merged_with_cuts" -> "SOS Name - Merged (With Cuts)"
      sosname <- parts[1]
      merged_idx <- which(parts == "merged")
      if (merged_idx < length(parts)) {
        type_parts <- parts[(merged_idx + 1):length(parts)]
        type_str <- paste(tools::toTitleCase(type_parts), collapse = " ")
        section_title <- sprintf("%s - Merged (%s)", sosname, type_str)
      } else {
        section_title <- sprintf("%s - Merged", sosname)
      }
    } else if (length(parts) >= 3) {
      # Handle classifier plots: "sosname_classifier_with_cuts"
      sosname <- parts[1]
      classifier <- parts[2]
      type_parts <- parts[3:length(parts)]
      type_str <- paste(tools::toTitleCase(type_parts), collapse = " ")
      section_title <- sprintf("%s - %s (%s)", sosname, classifier, type_str)
    } else {
      # Fallback: just use the plot name with underscores replaced
      section_title <- gsub("_", " ", plot_name)
    }

    # Create safe chunk name (no spaces, special chars)
    chunk_name <- paste0("plot_", gsub("[^a-zA-Z0-9_]", "_", plot_name))

    # Build the complete R Markdown template for this plot
    template <- paste0(
      "### ", section_title, "\n\n",
      backtick, backtick, backtick, "{r ", chunk_name,
      ", fig.height=", fig_height, ", fig.width=", fig_width, ", warning=FALSE}\n",
      plot_list_var_name, "[['", plot_name, "']]\n",
      backtick, backtick, backtick, "\n\n"
    )

    # Append chunk to output file
    cat(template, file = output_file, append = TRUE)
  }

  cat(sprintf("Created %d chunks in: %s\n", length(plot_names), output_file))
  return(output_file)
}


#' Generate R Markdown Chunks for Histogram Plots
#'
#' A convenience wrapper around \code{\link{brew_plot_chunks}} with preset
#' defaults optimized for histogram visualizations. Uses standard variable
#' naming and figure dimensions appropriate for frequency distribution plots.
#'
#' @param plot_list A named list of histogram plot objects. Names should follow
#'   the SOS analysis naming convention (e.g., \code{"RR_LYRAE_DSCTF_with_cut"}).
#' @param output_file Character string specifying the output file path.
#'   If \code{NULL} (default), creates a temporary file with prefix
#'   \code{"brewed_histogram_chunks_"}.
#'
#' @return Character string containing the path to the generated R Markdown
#'   file, or \code{NULL} if the plot list is empty.
#'
#' @details
#' This function applies the following defaults:
#' \itemize{
#'   \item \code{plot_list_var_name = "all_histogram_plots"}: Assumes the
#'     standard variable name from the histogram analysis pipeline
#'   \item \code{fig_height = 10}: Tall figures to accommodate frequency bars
#'     and cumulative sum overlays
#'   \item \code{fig_width = 14}: Wide figures for probability axis detail
#' }
#'
#' @examples
#' \dontrun{
#' # After generating histogram plots
#' all_histogram_plots <- plot_histograms_from_results(results)
#'
#' # Generate Rmd chunks
#' chunk_file <- brew_histogram_chunks(all_histogram_plots)
#'
#' # Or specify custom output location
#' chunk_file <- brew_histogram_chunks(
#'   all_histogram_plots,
#'   output_file = "histograms.Rmd"
#' )
#' }
#'
#' @seealso \code{\link{brew_plot_chunks}} for the underlying implementation,
#'   \code{\link{plot_histograms_from_results}} for generating the plot list
#'
#' @export
brew_histogram_chunks <- function(plot_list, output_file = NULL) {
  brew_plot_chunks(
    plot_list = plot_list,
    plot_list_var_name = "all_histogram_plots",
    output_file = output_file,
    fig_height = 10,
    fig_width = 14,
    file_pattern = "brewed_histogram_chunks_"
  )
}


#' Generate R Markdown Chunks for Skymap Plots
#'
#' A convenience wrapper around \code{\link{brew_plot_chunks}} with preset
#' defaults optimized for astronomical skymap visualizations. Uses figure
#' dimensions appropriate for HEALPix projections and celestial coordinate
#' displays.
#'
#' @param plot_list A named list of skymap plot objects. Names should follow
#'   the SOS analysis naming convention.
#' @param output_file Character string specifying the output file path.
#'   If \code{NULL} (default), creates a temporary file with prefix
#'   \code{"brewed_skymap_chunks_"}.
#'
#' @return Character string containing the path to the generated R Markdown
#'   file, or \code{NULL} if the plot list is empty.
#'
#' @details
#' This function applies the following defaults:
#' \itemize{
#'   \item \code{plot_list_var_name = "all_skymap_plots"}: Assumes the
#'     standard variable name from the skymap analysis pipeline
#'   \item \code{fig_height = 8}: Moderate height for map projections
#'   \item \code{fig_width = 12}: Wide format for full-sky coverage
#' }
#'
#' Skymap plots typically use Mollweide or other equal-area projections
#' that benefit from wider aspect ratios to display the full celestial sphere.
#'
#' @examples
#' \dontrun{
#' # After generating skymap plots
#' all_skymap_plots <- generate_skymaps(results)
#'
#' # Generate Rmd chunks
#' chunk_file <- brew_skymap_chunks(all_skymap_plots)
#' }
#'
#' @seealso \code{\link{brew_plot_chunks}} for the underlying implementation
#'
#' @export
brew_skymap_chunks <- function(plot_list, output_file = NULL) {
  brew_plot_chunks(
    plot_list = plot_list,
    plot_list_var_name = "all_skymap_plots",
    output_file = output_file,
    fig_height = 8,
    fig_width = 12,
    file_pattern = "brewed_skymap_chunks_"
  )
}


#' Generate R Markdown Chunks for Interactive Plotly Plots
#'
#' Creates an R Markdown file containing code chunks for rendering interactive
#' plotly visualizations. Unlike \code{\link{brew_plot_chunks}}, this function
#' omits figure dimension parameters since plotly handles sizing dynamically
#' through its JavaScript rendering engine.
#'
#' @param plot_list A named list of plotly objects. Names should follow the
#'   convention \code{"sosname_classifier_type"} or \code{"sosname_merged_type"}
#'   for automatic section title generation.
#' @param plot_list_var_name Character string specifying the variable name
#'   that will reference the plot list in the generated chunks. This must
#'   match the actual variable name in the R environment where the Rmd will
#'   be knitted.
#' @param output_file Character string specifying the output file path.
#'   If \code{NULL} (default), creates a temporary file in the current
#'   working directory.
#' @param file_pattern Character string specifying the prefix for temporary
#'   file names when \code{output_file} is \code{NULL}. Default is
#'   \code{"brewed_plotly_chunks_"}.
#'
#' @return Character string containing the path to the generated R Markdown
#'   file, or \code{NULL} if the plot list is empty.
#'
#' @details
#' This function differs from \code{\link{brew_plot_chunks}} in the following ways:
#' \itemize{
#'   \item No \code{fig.height} or \code{fig.width} chunk options are set
#'   \item Chunk names are prefixed with "plotly_" instead of "plot_"
#'   \item Designed for HTML output where plotly's responsive sizing works best
#' }
#'
#' \strong{Plotly Sizing:}
#' Plotly figures determine their dimensions through:
#' \itemize{
#'   \item The \code{layout()} specification in the plot object
#'   \item CSS styling of the containing HTML element
#'   \item Browser viewport when using responsive mode
#' }
#'
#' For fixed dimensions, set them in the plotly object itself using
#' \code{layout(width = ..., height = ...)}.
#'
#' \strong{HTML Output Requirement:}
#' Plotly chunks require HTML output format. When knitting to PDF or Word,
#' consider using \code{webshot} or \code{kaleido} for static image conversion.
#'
#' @examples
#' \dontrun{
#' # Create plotly plots
#' my_plotly_plots <- list(
#'   "RR_LYRAE_DSCTF_with_cut" = plot_ly(...),
#'   "RR_LYRAE_merged_no_cut" = plot_ly(...)
#' )
#'
#' # Generate Rmd chunks
#' chunk_file <- brew_plotly_chunks(
#'   plot_list = my_plotly_plots,
#'   plot_list_var_name = "my_plotly_plots",
#'   output_file = "plotly_sections.Rmd"
#' )
#'
#' # For histogram plotly plots specifically
#' chunk_file <- brew_plotly_chunks(
#'   plot_list = all_histogram_plots,
#'   plot_list_var_name = "all_histogram_plots"
#' )
#' }
#'
#' @seealso \code{\link{brew_plot_chunks}} for static plots with figure dimensions,
#'   \code{\link{plot_histogram_from_dataframe}} for creating plotly histogram objects
#'
#' @importFrom tools toTitleCase
#'
#' @export
brew_plotly_chunks <- function(plot_list,
                               plot_list_var_name,
                               output_file = NULL,
                               file_pattern = "brewed_plotly_chunks_") {

  # Create temp file if no output specified
  if (is.null(output_file)) {
    output_file <- tempfile(
      tmpdir = tempdir(),
      pattern = file_pattern,
      fileext = ".Rmd"
    )
  }

  # Clear file if it exists to ensure clean output
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # Extract plot names for iteration
  plot_names <- names(plot_list)

  # Early return with warning if no plots to process
  if (length(plot_names) == 0) {
    warning("No plots found in the list")
    return(NULL)
  }

  # Store backtick character for building code fence syntax
  backtick <- "`"

  # Iterate through each plot and generate corresponding Rmd chunk
  for (i in seq_along(plot_names)) {
    plot_name <- plot_names[i]

    # Parse plot name to create readable section title
    # Uses same logic as brew_plot_chunks for consistency
    parts <- strsplit(plot_name, "_")[[1]]

    # Create section title based on plot naming convention
    if ("merged" %in% parts) {
      # Handle merged plots: "sosname_merged_with_cuts" -> "SOS Name - Merged (With Cuts)"
      sosname <- parts[1]
      type_parts <- parts[3:length(parts)]
      type_str <- paste(toTitleCase(type_parts), collapse = " ")
      section_title <- sprintf("%s - Merged (%s)", sosname, type_str)
    } else {
      # Handle classifier plots: "sosname_classifier_with_cuts"
      sosname <- parts[1]
      classifier <- parts[2]
      type_parts <- parts[3:length(parts)]
      type_str <- paste(toTitleCase(type_parts), collapse = " ")
      section_title <- sprintf("%s - %s (%s)", sosname, classifier, type_str)
    }

    # Create safe chunk name with "plotly_" prefix
    # Distinguishes from static plot chunks in the same document
    chunk_name <- paste0("plotly_", gsub("[^a-zA-Z0-9_]", "_", plot_name))

    # Build the R Markdown template for this plotly plot
    # Note: No fig.height/fig.width - plotly handles sizing via JavaScript
    template <- paste0(
      "### ", section_title, "\n\n",
      backtick, backtick, backtick, "{r ", chunk_name, ", warning=FALSE}\n",
      plot_list_var_name, "[['", plot_name, "']]\n",
      backtick, backtick, backtick, "\n\n"
    )

    # Append chunk to output file
    cat(template, file = output_file, append = TRUE)
  }

  cat(sprintf("Created %d Plotly chunks in: %s\n", length(plot_names), output_file))
  return(output_file)
}


#' Generate R Markdown Chunks for Plotly Histogram Plots
#'
#' A convenience wrapper around \code{\link{brew_plotly_chunks}} with preset
#' defaults for interactive histogram visualizations from the SOS analysis
#' pipeline.
#'
#' @param plot_list A named list of plotly histogram objects. Names should
#'   follow the SOS analysis naming convention.
#' @param output_file Character string specifying the output file path.
#'   If \code{NULL} (default), creates a temporary file.
#'
#' @return Character string containing the path to the generated R Markdown
#'   file, or \code{NULL} if the plot list is empty.
#'
#' @details
#' This function assumes the standard variable name \code{"all_histogram_plots"}
#' from the histogram analysis pipeline. The generated chunks are suitable for
#' HTML R Markdown output with interactive hover, zoom, and pan capabilities.
#'≥ý
#' @examples
#' \dontrun{
#' # After generating plotly histogram plots
#' all_histogram_plots <- plot_histograms_from_results(results)
#'
#' # Generate Rmd chunks for interactive HTML report
#' chunk_file <- brew_plotly_histogram_chunks(all_histogram_plots)
#' }
#'
#' @seealso \code{\link{brew_plotly_chunks}} for the underlying implementation,
#'   \code{\link{plot_histogram_from_dataframe}} for creating the plotly objects
#'
#' @export
brew_plotly_histogram_chunks <- function(plot_list,   output_file = NULL) {
  brew_plotly_chunks(
    plot_list = plot_list,
    plot_list_var_name = deparse(substitute(plot_list)),
    output_file = output_file,
    file_pattern = "brewed_plotly_histogram_chunks_"
  )
}



library(plotly)
library(dplyr)

#' Generate Plotly Histograms from Histogram Analysis Results
#'
#' @param hist_results Results from run_histogram_analysis() or a data frame with histogram data
#' @param log_scale_threshold Ratio threshold for auto log scale (default 100)
#' @param bar_color Color for histogram bars (default 'rgba(55, 128, 191, 0.7)')
#' @param line_color Color for cumulative line (default 'rgba(55, 128, 191, 1.0)')
#' @return Named list of plotly histogram objects
#'
#' @export
generate_histogram_plots <- function(hist_results,
                                     log_scale_threshold = 100,
                                     bar_color = 'rgba(55, 128, 191, 0.7)',
                                     line_color = 'rgba(55, 128, 191, 1.0)') {

  # Extract the histogram data frame from results
  if (is.list(hist_results) && !is.data.frame(hist_results)) {
    if (!is.null(hist_results$histogram_for_viz)) {
      hist_df <- hist_results$histogram_for_viz
    } else if (!is.null(hist_results$combined_histograms)) {
      hist_df <- hist_results$combined_histograms
    } else {
      stop("Cannot find histogram data in hist_results")
    }
  } else {
    hist_df <- hist_results
  }

  # Ensure numeric types
  hist_df <- hist_df %>%
    mutate(
      freq = as.numeric(freq),
      nan_count = as.numeric(nan_count),
      non_nan_count = as.numeric(non_nan_count),
      bucket_avg = as.numeric(bucket_avg),
      global_min = as.numeric(global_min),
      global_max = as.numeric(global_max)
    )

  # Helper function to determine if log scale should be used
  should_use_log_scale <- function(df_subset) {
    freq_range <- range(df_subset$freq, na.rm = TRUE)
    if (freq_range[1] > 0 && freq_range[2] / freq_range[1] > log_scale_threshold) {
      return(TRUE)
    }
    return(FALSE)
  }

  # Function to create histogram for a single attribute
  create_single_histogram <- function(data, tbl_name, col_name) {

    df_subset <- data %>%
      filter(table_name == !!tbl_name,
             column_name == !!col_name)

    if (nrow(df_subset) == 0) {
      warning(sprintf("No data found for %s.%s", tbl_name, col_name))
      return(NULL)
    }

    # Calculate cumulative frequency
    df_subset <- df_subset %>%
      arrange(bucket) %>%
      mutate(cumulative_freq = cumsum(freq))

    # Get metadata for subtitle
    nan_count <- df_subset$nan_count[1]
    non_nan_count <- df_subset$non_nan_count[1]

    # Determine if log scale should be used
    use_log_scale <- should_use_log_scale(df_subset)

    # Create the plot
    fig <- plot_ly()

    # Add histogram bars
    fig <- fig %>%
      add_bars(
        data = df_subset,
        x = ~bucket_avg,
        y = ~freq,
        name = 'Frequency',
        marker = list(
          color = bar_color,
          line = list(color = line_color, width = 1)
        ),
        hovertemplate = paste(
          '<b>Bucket:</b> %{x:.2f}<br>',
          '<b>Frequency:</b> %{y}<br>',
          '<extra></extra>'
        )
      )

    # Add cumulative frequency line
    fig <- fig %>%
      add_lines(
        data = df_subset,
        x = ~bucket_avg,
        y = ~cumulative_freq,
        name = 'Cumulative',
        yaxis = 'y2',
        line = list(color = line_color, width = 2),
        hovertemplate = paste(
          '<b>Bucket:</b> %{x:.2f}<br>',
          '<b>Cumulative:</b> %{y}<br>',
          '<extra></extra>'
        )
      )

    # Layout with title and subtitle
    fig <- fig %>%
      layout(
        title = list(
          text = paste0(
            '<b>', col_name, '</b><br>',
            '<sup>', tbl_name,
            ' | Non-NaN: ', format(non_nan_count, big.mark = ','),
            ' | NaN: ', format(nan_count, big.mark = ','), '</sup>'
          ),
          font = list(size = 16)
        ),
        xaxis = list(title = 'Value'),
        yaxis = list(
          title = 'Frequency',
          side = 'left',
          type = if(use_log_scale) 'log' else 'linear'
        ),
        yaxis2 = list(
          title = 'Cumulative Count',
          overlaying = 'y',
          side = 'right',
          type = if(use_log_scale) 'log' else 'linear'
        ),
        hovermode = 'x unified',
        showlegend = TRUE,
        legend = list(x = 0.8, y = 1)
      )

    return(fig)
  }

  # Get unique table and column combinations
  table_col_combinations <- hist_df %>%
    select(table_name, column_name) %>%
    distinct()

  cat(sprintf("Generating %d histogram plots...\n", nrow(table_col_combinations)))

  # Generate all histograms
  histogram_list <- list()
  for (i in 1:nrow(table_col_combinations)) {
    tbl <- table_col_combinations$table_name[i]
    col <- table_col_combinations$column_name[i]

    histogram_list[[i]] <- create_single_histogram(hist_df, tbl, col)
  }

  # Add meaningful names to the list
  names(histogram_list) <- paste(
    table_col_combinations$table_name,
    table_col_combinations$column_name,
    sep = "."
  )

  # Remove any NULL entries
  histogram_list <- histogram_list[!sapply(histogram_list, is.null)]

  cat(sprintf("Generated %d histogram plots\n", length(histogram_list)))

  return(histogram_list)
}

# ============================================================================
# USAGE EXAMPLE
# ============================================================================
#
# # Generate plots from analysis results
# histogram_list <- generate_histogram_plots(sosHistAll)
#
# # Or with custom settings
# histogram_list <- generate_histogram_plots(
#   sosHistAll,
#   log_scale_threshold = 50,
#   bar_color = 'rgba(100, 150, 200, 0.7)',
#   line_color = 'rgba(100, 150, 200, 1.0)'
# )
#
# # Display a specific histogram
# histogram_list[["sos_cepheidsattributes.frequency"]]
#
# # Use with your brewing function
# brew_plotly_histogram_chunks <- function(plot_list, output_file = NULL) {
#   brew_plotly_chunks(
#     plot_list = plot_list,
#     plot_list_var_name = "histogram_list",
#     output_file = output_file,
#     file_pattern = "brewed_plotly_histogram_chunks_"
#   )
# }
#
