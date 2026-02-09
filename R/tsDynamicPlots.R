#' @title Timeseries Visualization Utilities
#' @description Functions for creating interactive plotly visualizations of
#'   astronomical timeseries data, including raw (derived) light curves and
#'   phase-folded plots. Supports multi-source grid layouts with Gaia
#'   photometric band color coding (G/BP/RP).
#' @name timeseries-plots
#' @keywords internal
NULL

library(plotly)
library(htmlwidgets)
library(dplyr)

#' Create Maximizable Multi-Source Timeseries Grid with Auto-Height
#'
#' @param ts.all Timeseries data frame
#' @param periodSet Period information data frame
#' @param plot_height Total height in pixels. If NULL, auto-calculated.
#' @param height_per_source Height in pixels per source for auto-calculation. Default 250.
#'
#' @return A plotly object with height set
#'
#' @importFrom htmlwidgets onRender
#'
#' @export
#'
create_maximizable_plots <- function(ts.all,
                                     periodSet,
                                     plot_height = NULL,
                                     height_per_source = 250) {

  # Auto-calculate height if not provided
  if (is.null(plot_height)) {
    n_sources <- length(unique(ts.all$sourceid))
    plot_height <- n_sources * height_per_source
  }

  f <- ts.all %>%
    mutate(sourceid = as.integer64(sourceid)) %>%
    inner_join(periodSet, by = "sourceid")

  g <- f %>%
    group_by(sourceid, tstag) %>%
    do(get_folds(.))

  create_color_mapping <- function(tstags) {
    color_map <- character(length(tstags))
    names(color_map) <- tstags

    for(tag in tstags) {
      if(grepl("_G$", tag)) {
        color_map[tag] <- "darkgrey"
      } else if(grepl("_RP$", tag)) {
        color_map[tag] <- "red"
      } else if(grepl("_BP$", tag)) {
        color_map[tag] <- "blue"
      } else {
        color_map[tag] <- "black"
      }
    }
    return(color_map)
  }

  sourceids <- unique(ts.all$sourceid)
  color_map <- create_color_mapping(unique(ts.all$tstag))

  # --- Build derived (raw) timeseries subplots ---
  derived_subplots <- list()
  for(i in seq_along(sourceids)) {
    source_data <- ts.all %>% filter(sourceid == sourceids[i])

    p <- plot_ly(data = source_data,
                 x = ~obstime,
                 y = ~val,
                 error_y = list(
                   type = "data",
                   symmetric = FALSE,
                   array = ~valerr,
                   arrayminus = ~valerr,
                   thickness = 1,
                   width = 2
                 ),
                 color = ~tstag,
                 colors = color_map,
                 type = "scatter",
                 mode = "markers",
                 marker = list(size = 1),
                 showlegend = (i == 1),
                 name = ~tstag) %>%
      layout(
        xaxis = list(title = if(i == length(sourceids)) "JD" else "", titlefont = list(size = 10)),
        yaxis = list(title = "Magnitude", autorange = "reversed", titlefont = list(size = 10)),
        annotations = list(
          list(x = 0.5, y = 1.02, text = paste("Source ID:", sourceids[i]),
               xref = "paper", yref = "paper", xanchor = "center", showarrow = FALSE)
        )
      )

    derived_subplots[[i]] <- p
  }

  # --- Build folded (phase) timeseries subplots ---
  folded_subplots <- list()
  for(i in seq_along(sourceids)) {
    source_data <- g %>% filter(sourceid == sourceids[i])
    period_info <- periodSet %>% filter(sourceid == sourceids[i])

    p <- plot_ly(data = source_data,
                 x = ~phases,
                 y = ~magnitudes,
                 error_y = list(
                   type = "data",
                   symmetric = FALSE,
                   array = ~errors,
                   arrayminus = ~errors,
                   thickness = 1,
                   width = 2
                 ),
                 color = ~tstag,
                 colors = color_map,
                 type = "scatter",
                 mode = "markers",
                 marker = list(size = 1),
                 showlegend = FALSE,
                 name = ~tstag) %>%
      layout(
        xaxis = list(title = if(i == length(sourceids)) "Phase" else "", titlefont = list(size = 10)),
        yaxis = list(title = "Magnitude", autorange = "reversed", titlefont = list(size = 10)),
        shapes = list(
          list(
            type = "line",
            x0 = 1, x1 = 1,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "black", width = 1, dash = "dash")
          )
        ),
        annotations = list(
          list(x = 0.5, y = 1.02, text = paste("Source ID:", sourceids[i]),
               xref = "paper", yref = "paper", xanchor = "center", showarrow = FALSE),
          list(x = 0.95, y = 0.95,
               text = paste("Period:", round(period_info$period[1], 4)),
               xref = "paper", yref = "paper", xanchor = "right", showarrow = FALSE,
               font = list(size = 8))
        )
      )

    folded_subplots[[i]] <- p
  }

  # --- Combine subplots ---
  derived_grid <- subplot(derived_subplots, nrows = length(sourceids), shareX = TRUE, titleY = TRUE,
                          margin = 0.01)
  folded_grid <- subplot(folded_subplots, nrows = length(sourceids), shareX = TRUE, titleY = TRUE,
                         margin = 0.01)

  main_plot <- subplot(derived_grid, folded_grid, nrows = 1, shareY = FALSE, titleX = TRUE,
                       margin = 0.02) %>%
    layout(
      title = list(text = "Timeseries Analysis (Shift+Click on ts point to maximize)", x = 0.5, font = list(size = 16)),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center",
                    font = list(size = 14)),
      margin = list(l = 50, r = 50, t = 80, b = 120)
    )
  # --- Attach JavaScript for shift+click maximize AND synchronized legend ---
  main_plot <- main_plot %>%
    onRender("
      function(el, x) {

        // --- Synchronized legend click handler ---
        el.on('plotly_legendclick', function(data) {
          var clickedName = data.data[data.curveNumber].name;
          var currentVisibility = data.data[data.curveNumber].visible;

          // Determine new visibility state
          // visible can be: true, false, 'legendonly', or undefined
          var newVisibility;
          if (currentVisibility === 'legendonly' || currentVisibility === false) {
            newVisibility = true;
          } else {
            newVisibility = 'legendonly';
          }

          // Find all traces with the same name (same tstag across all subplots)
          var update = {visible: []};
          var traceIndices = [];

          for (var i = 0; i < data.data.length; i++) {
            if (data.data[i].name === clickedName) {
              traceIndices.push(i);
              update.visible.push(newVisibility);
            }
          }

          // Apply visibility to all matching traces
          Plotly.restyle(el, {visible: newVisibility}, traceIndices);

          // Return false to prevent default legend click behavior
          return false;
        });

        // --- Shift+click maximize handler ---
        el.on('plotly_click', function(data) {
          if (data.event.shiftKey) {
            var point = data.points[0];
            var plotData = point.data;

            var maximizedDiv = document.createElement('div');
            maximizedDiv.id = 'maximized-plot';
            maximizedDiv.style.position = 'fixed';
            maximizedDiv.style.top = '0';
            maximizedDiv.style.left = '0';
            maximizedDiv.style.width = '100vw';
            maximizedDiv.style.height = '100vh';
            maximizedDiv.style.backgroundColor = 'white';
            maximizedDiv.style.zIndex = '9999';
            maximizedDiv.style.padding = '20px';
            maximizedDiv.style.boxSizing = 'border-box';

            var closeBtn = document.createElement('button');
            closeBtn.innerHTML = 'Close (ESC or Shift+Click again)';
            closeBtn.style.position = 'absolute';
            closeBtn.style.top = '10px';
            closeBtn.style.right = '10px';
            closeBtn.style.zIndex = '10000';
            closeBtn.style.padding = '10px';
            closeBtn.style.cursor = 'pointer';
            closeBtn.style.backgroundColor = '#f0f0f0';
            closeBtn.style.border = '1px solid #ccc';
            closeBtn.style.borderRadius = '4px';

            var plotDiv = document.createElement('div');
            plotDiv.style.width = '100%';
            plotDiv.style.height = 'calc(100% - 60px)';
            plotDiv.style.marginTop = '50px';

            maximizedDiv.appendChild(closeBtn);
            maximizedDiv.appendChild(plotDiv);
            document.body.appendChild(maximizedDiv);

            var newPlotData = [{
              x: plotData.x,
              y: plotData.y,
              mode: 'markers',
              type: 'scatter',
              marker: {
                size: 6,
                color: plotData.marker.color || (
                  plotData.name.includes('_G') ? 'darkgrey' :
                  plotData.name.includes('_RP') ? 'red' :
                  plotData.name.includes('_BP') ? 'blue' : 'black'
                )
              },
              name: plotData.name || 'Data',
              error_y: plotData.error_y
            }];

            var layout = {
              title: {
                text: 'Maximized Plot - ' + (plotData.name || 'Subplot'),
                x: 0.5,
                font: { size: 18 }
              },
              xaxis: {
                title: 'Time/Phase',
                titlefont: { size: 14 }
              },
              yaxis: {
                title: 'Magnitude',
                autorange: 'reversed',
                titlefont: { size: 14 }
              },
              showlegend: true,
              legend: {
                orientation: 'h',
                x: 0.5,
                y: -0.1,
                xanchor: 'center'
              }
            };

            Plotly.newPlot(plotDiv, newPlotData, layout);

            closeBtn.onclick = function() {
              document.body.removeChild(maximizedDiv);
            };

            var escHandler = function(e) {
              if (e.key === 'Escape') {
                if (document.getElementById('maximized-plot')) {
                  document.body.removeChild(maximizedDiv);
                  document.removeEventListener('keydown', escHandler);
                }
              }
            };
            document.addEventListener('keydown', escHandler);
          }
        });
      }
    ")

  # Set height directly on the plotly object (not in layout)
  main_plot$height <- plot_height
  main_plot$width <- NULL

  return(main_plot)
}


#' Create Simple Multi-Source Timeseries Plot
#'
#' Generates a simpler interactive plotly visualization combining derived
#' (raw) and phase-folded light curves for multiple sources. Unlike
#' \code{\link{create_maximizable_plots}}, all sources are overlaid on
#' two panels (derived and folded) rather than arranged in a per-source
#' grid. This produces a lighter, more reliable output without JavaScript
#' dependencies.
#'
#' @param ts.all A data frame containing raw timeseries data with columns:
#'   \describe{
#'     \item{sourceid}{Numeric or character Gaia source identifier}
#'     \item{obstime}{Numeric observation time in Julian Date (JD)}
#'     \item{val}{Numeric magnitude value}
#'     \item{valerr}{Numeric magnitude error (symmetric)}
#'     \item{tstag}{Character timeseries tag identifying the photometric
#'       band. Suffix determines color: \code{_G} -> darkgrey,
#'       \code{_RP} -> red, \code{_BP} -> blue}
#'   }
#' @param g A data frame containing phase-folded timeseries data with columns:
#'   \describe{
#'     \item{sourceid}{Numeric or character Gaia source identifier}
#'     \item{phases}{Numeric phase values (0 to 2)}
#'     \item{magnitudes}{Numeric magnitude values}
#'     \item{errors}{Numeric magnitude errors}
#'     \item{tstag}{Character timeseries tag (same convention as \code{ts.all})}
#'   }
#' @param periodSet A data frame containing period information with columns:
#'   \describe{
#'     \item{sourceid}{Numeric or character Gaia source identifier}
#'     \item{period}{Numeric period value in days}
#'   }
#'   Note: period annotations are not displayed in this simplified version.
#'
#' @return A plotly object containing the combined two-panel subplot.
#'
#' @details
#' \strong{Layout:}
#' The plot is organized as a 1 x 2 grid:
#' \itemize{
#'   \item Left panel: Raw timeseries (JD vs. magnitude) with all sources overlaid
#'   \item Right panel: Phase-folded curves with all sources overlaid
#' }
#'
#' Y-axes are independently scaled between panels and reversed (brighter
#' magnitudes at top, following astronomical convention).
#'
#' \strong{Color Mapping:}
#' Uses the same Gaia photometric band color scheme as
#' \code{\link{create_maximizable_plots}}:
#' \itemize{
#'   \item \code{_G} suffix: darkgrey (Gaia G-band)
#'   \item \code{_RP} suffix: red (Gaia RP-band)
#'   \item \code{_BP} suffix: blue (Gaia BP-band)
#'   \item Other: black (fallback)
#' }
#'
#' \strong{Legend:}
#' Each trace is labeled as \code{"tstag - Source sourceid"} for
#' identification. Legend entries appear only on the derived (left) panel
#' to avoid duplication.
#'
#' \strong{Folded Plot Features:}
#' A dashed vertical line at phase = 1 separates the first and second
#' period repetitions.
#'
#' \strong{When to Use:}
#' Prefer this function over \code{\link{create_maximizable_plots}} when:
#' \itemize{
#'   \item Number of sources is small (< 5) and a grid layout is unnecessary
#'   \item HTML output with custom JavaScript is not available (e.g., PDF knitting)
#'   \item A simpler, more robust visualization is preferred
#' }
#'
#' @examples
#' \dontrun{
#' # Create simple overlay plot
#' fig <- create_simple_plots(ts.all, g, periodSet)
#' fig
#'
#' # Save as HTML
#' htmlwidgets::saveWidget(fig, "timeseries_simple.html", selfcontained = TRUE)
#' }
#'
#' @seealso \code{\link{create_maximizable_plots}} for per-source grid layout
#'   with shift+click maximize
#'
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom dplyr filter
#'
#' @export
#'
create_simple_plots <- function(ts.all, g, periodSet) {

  #' @describeIn create_maximizable_plots Internal color mapping helper
  #' @keywords internal
  create_color_mapping <- function(tstags) {
    color_map <- character(length(tstags))
    names(color_map) <- tstags

    for(tag in tstags) {
      if(grepl("_G$", tag)) {
        color_map[tag] <- "darkgrey"     # Gaia G-band
      } else if(grepl("_RP$", tag)) {
        color_map[tag] <- "red"          # Gaia RP-band
      } else if(grepl("_BP$", tag)) {
        color_map[tag] <- "blue"         # Gaia BP-band
      } else {
        color_map[tag] <- "black"        # Unrecognized band fallback
      }
    }
    return(color_map)
  }

  # Extract unique source IDs and build color mapping
  sourceids <- unique(ts.all$sourceid)
  color_map <- create_color_mapping(unique(ts.all$tstag))

  # --- Build derived (raw) timeseries panel ---
  # All sources overlaid on a single plot
  derived_plot <- plot_ly() %>%
    layout(
      title = list(text = "Timeseries", x = 0.5, font = list(size = 14)),
      showlegend = TRUE
    )

  # Add one trace per source to allow per-source legend toggling
  for(sid in sourceids) {
    source_data <- ts.all %>% filter(sourceid == sid)

    derived_plot <- derived_plot %>%
      add_trace(
        data = source_data,
        x = ~obstime,
        y = ~val,
        error_y = list(
          type = "data",
          symmetric = FALSE,
          array = ~valerr,
          arrayminus = ~valerr,
          thickness = 1,
          width = 2
        ),
        color = ~tstag,
        colors = color_map,
        type = "scatter",
        mode = "markers",
        marker = list(size = 2),
        # Label includes both band tag and source ID for identification
        name = ~paste(tstag, "- Source", sid),
        showlegend = TRUE
      )
  }

  # Configure axes with reversed y for astronomical convention
  derived_plot <- derived_plot %>%
    layout(
      xaxis = list(title = "JD"),
      yaxis = list(title = "Magnitude", autorange = "reversed")
    )

  # --- Build folded (phase) timeseries panel ---
  # All sources overlaid, legend suppressed (already shown in derived panel)
  folded_plot <- plot_ly() %>%
    layout(
      title = list(text = "Folded Timeseries", x = 0.5, font = list(size = 14)),
      showlegend = TRUE
    )

  for(sid in sourceids) {
    source_data <- g %>% filter(sourceid == sid)

    folded_plot <- folded_plot %>%
      add_trace(
        data = source_data,
        x = ~phases,
        y = ~magnitudes,
        error_y = list(
          type = "data",
          symmetric = FALSE,
          array = ~errors,
          arrayminus = ~errors,
          thickness = 1,
          width = 2
        ),
        color = ~tstag,
        colors = color_map,
        type = "scatter",
        mode = "markers",
        marker = list(size = 2),
        name = ~paste(tstag, "- Source", sid),
        # Suppress duplicate legend entries
        showlegend = FALSE
      )
  }

  # Configure folded panel with phase=1 separator line
  folded_plot <- folded_plot %>%
    layout(
      xaxis = list(title = "Phase"),
      yaxis = list(title = "Magnitude", autorange = "reversed"),
      # Dashed vertical line at phase = 1 separating the two period cycles
      shapes = list(
        list(
          type = "line",
          x0 = 1, x1 = 1,
          y0 = 0, y1 = 1,
          yref = "paper",
          line = list(color = "black", width = 1, dash = "dash")
        )
      )
    )

  # --- Combine panels side-by-side ---
  # Y-axes are independent between panels (different value ranges possible)
  combined_plot <- subplot(derived_plot, folded_plot, nrows = 1, shareY = FALSE, titleX = TRUE,
                           margin = 0.02) %>%
    layout(
      title = list(text = "Timeseries Analysis", x = 0.5, font = list(size = 16)),
      margin = list(l = 50, r = 50, t = 80, b = 120),
      # Horizontal legend centered below the plots
      legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center",
                    font = list(size = 14))
    )

  return(combined_plot)
}

get_folds <- function(fd, refTime = 1666.43849021944516) {
  d = foldTimeseriesFull (fd$sourceid, fd$tstag, fd$period, fd$obstime, fd$val, fd$valerr, refTime)
  return(d);
}





# get period

#' Generate R Markdown Chunks for Paginated Timeseries Plots
#'
#' Creates an R Markdown file containing individual code chunks for each page
#' of timeseries plots. This approach is necessary because htmlwidgets (like
#' plotly) cannot be reliably rendered in loops during R Markdown knitting -
#' they get serialized as JSON text instead of interactive HTML widgets.
#'
#' By generating separate chunks, each widget is processed independently by
#' knitr's htmlwidget output hooks, ensuring proper rendering.
#'
#' @param n_pages Integer specifying the total number of pages to generate.
#'   Typically calculated as \code{ceiling(length(unique(ts.all$sourceid)) / sources_per_page)}.
#' @param sources_per_page Integer specifying the maximum number of sources
#'   to display per page. Default is 8. Recommended range: 5-10 for readability.
#' @param height_per_source Numeric height in pixels allocated per source in
#'   the plot. Default is 250. The total plot height for each page will be
#'   approximately \code{n_sources_on_page * height_per_source}.
#' @param output_file Character string specifying the output file path for
#'   the generated R Markdown chunks. If \code{NULL} (default), creates
#'   \code{ts_chunks_temp.Rmd} in the current working directory.
#'
#' @return Character string containing the path to the generated R Markdown
#'   file. This file is intended to be included in a parent R Markdown
#'   document using a child chunk.
#'
#' @details
#' \strong{Why This Function Exists:}
#' When rendering multiple plotly widgets in a loop during R Markdown knitting,
#' the widgets are serialized as JSON/text instead of being rendered as
#' interactive HTML. This is a known limitation of how knitr processes
#' htmlwidgets in loops. The only reliable solution is to place each widget
#' in its own code chunk.
#'
#' \strong{Generated Chunk Structure:}
#' Each generated chunk:
#' \enumerate{
#'   \item Calculates the source IDs for that page using index slicing
#'   \item Filters \code{ts.all} and \code{periodSet} to those sources
#'   \item Calls \code{\link{create_maximizable_plots}} with the filtered data
#'   \item Has \code{echo=FALSE, warning=FALSE, message=FALSE} options set
#' }
#'
#' \strong{Required Variables in Parent Document:}
#' The generated chunks assume the following variables exist in the
#' R environment when the child document is knitted:
#' \itemize{
#'   \item \code{ts.all}: Data frame with timeseries data
#'   \item \code{periodSet}: Data frame with period information
#' }
#'
#' \strong{Usage Pattern:}
#' \preformatted{
#' # In your R Markdown document:
#'
#' ```
#' {r generate_chunks}
#' sourceids <- unique(ts.all$sourceid)
#' sources_per_page <- 8
#' n_pages <- ceiling(length(sourceids) / sources_per_page)
#'
#' ts_chunks_file <- brew_timeseries_chunks(
#'   n_pages = n_pages,
#'   sources_per_page = sources_per_page,
#'   height_per_source = 250
#' )
#' ```
#'
#' ```
#' {r child=ts_chunks_file}
#' ```
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate pagination parameters
#' sourceids <- unique(ts.all$sourceid)
#' sources_per_page <- 8
#' n_sources <- length(sourceids)
#' n_pages <- ceiling(n_sources / sources_per_page)
#'
#' # Generate the child Rmd file
#' ts_chunks_file <- brew_timeseries_chunks(
#'   n_pages = n_pages,
#'   sources_per_page = sources_per_page,
#'   height_per_source = 250
#' )
#'
#' # The file can then be included via child chunk:
#' # ```{r child=ts_chunks_file}
#' # ```
#'
#' # Or with custom output location
#' ts_chunks_file <- brew_timeseries_chunks(
#'   n_pages = 5,
#'   sources_per_page = 10,
#'   height_per_source = 200,
#'   output_file = "output/timeseries_pages.Rmd"
#' )
#' }
#'
#' @seealso \code{\link{create_maximizable_plots}} for the underlying plot function,
#'   \code{\link{brew_plot_chunks}} for a similar approach with static plots
#'
#' @export
brew_timeseries_chunks <- function(n_pages,
                                   sources_per_page,
                                   height_per_source = 250,
                                   output_file = NULL) {

  # Create default output file in current working directory if not specified
  if (is.null(output_file)) {
    output_file <- file.path(tempdir(), "ts_chunks_temp.Rmd")
  }

  # Remove existing file to ensure clean output
  # Prevents appending to stale content from previous runs
  if (file.exists(output_file)) file.remove(output_file)

  # Store backtick character for building code fence syntax
  # Using variable avoids escaping issues in string construction
  bt <- "`"

  # Generate one chunk per page
  for (page in seq_len(n_pages)) {
    # Calculate source index range for this page
    start_idx <- (page - 1) * sources_per_page + 1
    end_idx <- min(page * sources_per_page, n_pages * sources_per_page)

    # Build the complete R Markdown chunk for this page
    # Includes: level-2 header, code chunk with plot generation
    # The chunk:
    #   1. Extracts source IDs for this page via index slicing
    #   2. Filters ts.all and periodSet to those sources
    #   3. Calls create_maximizable_plots with height parameter
    #   4. Includes guard for empty data (nrow check)
    chunk <- paste0(
      "\n## Page ", page, " of ", n_pages, "\n\n",
      bt, bt, bt, "{r ts_page_", page, ", echo=FALSE, warning=FALSE, message=FALSE}\n",
      "page_sids <- unique(ts.all$sourceid)[", start_idx, ":", end_idx, "]\n",
      "ts_p <- ts.all %>% dplyr::filter(sourceid %in% page_sids)\n",
      "per_p <- periodSet %>% dplyr::filter(sourceid %in% page_sids)\n",
      "if(nrow(ts_p) > 0) create_maximizable_plots(ts_p, per_p, height_per_source = ", height_per_source, ")\n",
      bt, bt, bt, "\n\n"
    )

    # Append chunk to output file
    # Using append=TRUE allows sequential building of the document
    cat(chunk, file = output_file, append = TRUE)
  }

  return(output_file)
}
