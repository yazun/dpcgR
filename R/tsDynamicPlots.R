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


#' Create Maximizable Multi-Source Timeseries Grid
#'
#' Generates an interactive plotly grid combining derived (raw) and
#' phase-folded light curves for multiple sources. Each source occupies
#' a row in the grid, with derived timeseries on the left and folded
#' timeseries on the right. Supports shift+click to maximize individual
#' subplots in a full-screen overlay.
#'
#' @param ts.all A data frame containing raw timeseries data with columns:
#'   \describe{
#'     \item{sourceid}{Numeric or character Gaia source identifier}
#'     \item{obstime}{Numeric observation time in Julian Date (JD)}
#'     \item{val}{Numeric magnitude value}
#'     \item{valerr}{Numeric magnitude error (symmetric)}
#'     \item{tstag}{Character timeseries tag identifying the photometric
#'       band (e.g., \code{"FLUX_G"}, \code{"MAG_RP"}, \code{"MAG_BP"}).
#'       Suffix determines color: \code{_G} -> darkgrey, \code{_RP} -> red,
#'       \code{_BP} -> blue}
#'   }
#' @param g A data frame containing phase-folded timeseries data with columns:
#'   \describe{
#'     \item{sourceid}{Numeric or character Gaia source identifier}
#'     \item{phases}{Numeric phase values (0 to 2, with dashed line at phase = 1)}
#'     \item{magnitudes}{Numeric magnitude values}
#'     \item{errors}{Numeric magnitude errors}
#'     \item{tstag}{Character timeseries tag (same convention as \code{ts.all})}
#'   }
#' @param periodSet A data frame containing period information with columns:
#'   \describe{
#'     \item{sourceid}{Numeric or character Gaia source identifier}
#'     \item{period}{Numeric period value in days}
#'   }
#'
#' @return A plotly object containing the combined subplot grid with
#'   embedded JavaScript for shift+click maximization.
#'
#' @details
#' \strong{Grid Layout:}
#' The plot is organized as an N x 2 grid where N is the number of unique
#' sources. The left column shows raw timeseries (JD vs. magnitude), and
#' the right column shows phase-folded curves. Within each column, the
#' x-axis is shared across all rows. Y-axes are reversed (brighter = up,
#' standard astronomical convention).
#'
#' \strong{Color Mapping:}
#' Photometric bands are color-coded by the \code{tstag} suffix:
#' \itemize{
#'   \item \code{_G} suffix: darkgrey (Gaia G-band)
#'   \item \code{_RP} suffix: red (Gaia RP-band)
#'   \item \code{_BP} suffix: blue (Gaia BP-band)
#'   \item Other: black (fallback)
#' }
#'
#' \strong{Folded Plot Annotations:}
#' Each folded subplot includes:
#' \itemize{
#'   \item A dashed vertical line at phase = 1 separating the two periods
#'   \item A period annotation in the upper-right corner
#'   \item Source ID annotation above the plot
#' }
#'
#' \strong{Shift+Click Maximize:}
#' Clicking a data point while holding Shift opens a full-screen overlay
#' showing the clicked trace at larger scale. The overlay can be dismissed
#' via the close button, pressing ESC, or shift+clicking again. This feature
#' requires \code{htmlwidgets::onRender} and only works in HTML output.
#'
#' \strong{Legend Behavior:}
#' Only the first row of subplots displays legend entries to avoid
#' duplication. The legend is placed horizontally below the plot.
#'
#' @examples
#' \dontrun{
#' # Create maximizable grid from timeseries data
#' fig <- create_maximizable_plots(ts.all, g, periodSet)
#' fig
#'
#' # Save as self-contained HTML
#' htmlwidgets::saveWidget(fig, "timeseries_grid.html", selfcontained = TRUE)
#' }
#'
#' @seealso \code{\link{create_simple_plots}} for a simpler alternative
#'   without maximize functionality
#'
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom htmlwidgets onRender
#' @importFrom dplyr filter
#'
#' @export
create_maximizable_plots <- function(ts.all, g, periodSet) {


  #'
  #' Maps timeseries tags to colors based on their photometric band suffix.
  #'
  #' @param tstags Character vector of timeseries tag names.
  #' @return Named character vector mapping tags to color strings.
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

  # --- Build derived (raw) timeseries subplots ---
  # One subplot per source, stacked vertically with shared x-axis
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
                 # Only show legend for first row to avoid duplication
                 showlegend = (i == 1),
                 name = ~tstag) %>%
      layout(
        # Only label x-axis on bottom-most subplot
        xaxis = list(title = if(i == length(sourceids)) "JD" else "", titlefont = list(size = 10)),
        # Reversed y-axis: brighter magnitudes (lower values) at top
        yaxis = list(title = "Magnitude", autorange = "reversed", titlefont = list(size = 10)),
        # Source ID annotation above each subplot
        annotations = list(
          list(x = 0.5, y = 1.02, text = paste("Source ID:", sourceids[i]),
               xref = "paper", yref = "paper", xanchor = "center", showarrow = FALSE)
        )
      )

    derived_subplots[[i]] <- p
  }

  # --- Build folded (phase) timeseries subplots ---
  # One subplot per source, with period annotation and phase=1 marker
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
                 # Legend already shown in derived subplots
                 showlegend = FALSE,
                 name = ~tstag) %>%
      layout(
        xaxis = list(title = if(i == length(sourceids)) "Phase" else "", titlefont = list(size = 10)),
        yaxis = list(title = "Magnitude", autorange = "reversed", titlefont = list(size = 10)),
        # Dashed vertical line at phase = 1 separating the two periods
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
          # Source ID annotation above subplot
          list(x = 0.5, y = 1.02, text = paste("Source ID:", sourceids[i]),
               xref = "paper", yref = "paper", xanchor = "center", showarrow = FALSE),
          # Period value annotation in upper-right corner
          list(x = 0.95, y = 0.95,
               text = paste("Period:", round(period_info$period[1], 4)),
               xref = "paper", yref = "paper", xanchor = "right", showarrow = FALSE,
               font = list(size = 8))
        )
      )

    folded_subplots[[i]] <- p
  }

  # --- Combine subplots into final grid layout ---
  # Stack each type vertically with shared x-axis within columns
  derived_grid <- subplot(derived_subplots, nrows = length(sourceids), shareX = TRUE, titleY = TRUE,
                          margin = 0.01)
  folded_grid <- subplot(folded_subplots, nrows = length(sourceids), shareX = TRUE, titleY = TRUE,
                         margin = 0.01)

  # Place derived and folded grids side-by-side
  # Y-axes are NOT shared between columns (different value ranges possible)
  main_plot <- subplot(derived_grid, folded_grid, nrows = 1, shareY = FALSE, titleX = TRUE,
                       margin = 0.02) %>%
    layout(
      title = list(text = "Timeseries Analysis (Shift+Click on ts point to maximize)", x = 0.5, font = list(size = 16)),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center",
                    font = list(size = 14)),
      margin = list(l = 50, r = 50, t = 80, b = 120)
    )

  # --- Attach shift+click maximize JavaScript ---
  # Creates a full-screen overlay when user shift+clicks a data point
  # Overlay includes: close button, ESC key handler, re-plotted trace at full size
  main_plot <- main_plot %>%
    onRender("
      function(el, x) {
        el.on('plotly_click', function(data) {
          // Only trigger on shift+click
          if (data.event.shiftKey) {
            var point = data.points[0];
            var plotData = point.data;

            // Create full-screen overlay container
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

            // Close button in top-right corner
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

            // Plot container fills remaining space below close button
            var plotDiv = document.createElement('div');
            plotDiv.style.width = '100%';
            plotDiv.style.height = 'calc(100% - 60px)';
            plotDiv.style.marginTop = '50px';

            maximizedDiv.appendChild(closeBtn);
            maximizedDiv.appendChild(plotDiv);
            document.body.appendChild(maximizedDiv);

            // Re-create the clicked trace at full size with color mapping
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

            // Close on button click
            closeBtn.onclick = function() {
              document.body.removeChild(maximizedDiv);
            };

            // Close on ESC key press
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
