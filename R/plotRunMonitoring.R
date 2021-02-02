#' Creates 1D histogram plot based on Run data.
#'
#' PLots 1D histogram based on the Run data fetched beforehand
#'
#' @param histData data retrieved
#'
#' @return ggplot with 1D histogram
#' @export
#' @importFrom highcharter highchart hchart hc_chart hc_plotOptions hc_title hc_subtitle hc_exporting hc_xAxis hc_yAxis hc_yAxis_multiples hc_colorAxis hc_tooltip hc_add_series hcaes hc_legend
#'
#' @examples \dontrun{
#' # get data for a specific monitoring plot
#'```{sql connection = conn, output.var = "runStats", echo = FALSE}
#' select * from getrun1dhistogram(?inRunId, 'Stat G time: mean \(([a-zA-Z]+)')
#'```
#' # plot it.
#' entryRow<-runStats[1,]
#' seriesName<- entryRow[,"attribute"]
#' create1DHistogram(runStats)
#'}
create1DHistogram <- function(histData) {

  if(length(which (histData$val != 0 ))!=0) {
    histData<-histData[(min(which ( histData$val != 0 ))-1) : (max( which( histData$val != 0 ))+1),]
  }
  cumV = cumsum(histData$val)
  entryRow<-histData[1,]
  seriesName<- entryRow[,"attribute"]

  hc <- highchart(type = "chart") %>%
    hc_title(text = seriesName) %>%
    hc_subtitle(text = "") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_chart(zoomType = "x") %>%
    hc_xAxis(
      title = list(text = entryRow$xlabel),
      x = histData$x,
      crosshair = TRUE,
      plotLines = list(
        list(label = list(text = "median"),
             color = "#FF0000",
             width = .2,
             value = histData$x[which.min(abs(cumV/max(cumV)-0.5))]),
        list(label = list(text = "mean"),
             color = "#FF0000",
             width = .2,
             value = sum(histData$val*histData$x)/sum(histData$val))
      ),
      plotBands = list(
        list(
          label = list(text = "IQR", verticalAlign = "middle"),
          color = "rgba(255, 255, 0, 0.2)",
          from = histData$x[which.min(abs(cumV/max(cumV)-0.25))],
          to = histData$x[which.min(abs(cumV/max(cumV)-0.75))]
        ))) %>%
    hc_yAxis_multiples(
      list(type = ifelse( entryRow$scale == "LINEAR","", "logarithmic"), title = list(text = "Count"), crosshair = TRUE),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Cumulative"))
    ) %>%
    hc_tooltip(pointFormat = "For bin {point.x} there is <b>{point.y}</b> items<br/>", footerFormat = paste("All sources: <b> ", entryRow$count , "</b><p>Failed sources: ",entryRow$failed)) %>%
    hc_add_series(histData, hcaes(name = seriesName , y =  val) , name = seriesName, id = "Transits",  type = "column") %>%
    hc_add_series(histData, hcaes(name = paste("Cumulative",seriesName) , y =  cumsum(val)) , name =  paste("Cumulative",seriesName), id = "Cumulative",  type = "line", yAxis = 1)

  # knit_print_own.htmlwidget(hc, width="800px", height="600px")
  hc
}

#' Create a 2D histogram based on a Run data.
#'
#' Also out of a problematic (javascript-wise) data where we DO HAVE small fractional problem on the x,y axis.
#' @param run2Dmeta metadata about the plot
#' @param run2Ddata dataframe with the plot data
#'
#' @return highcharts 2D histogram plot
#' @export
#'
#' @examples \dontrun{
#' # Get both metadata and data via two separate queries
#' ```{sql connection = conn, output.var = "run2Dmeta", echo = FALSE}
#' select * From getRun2DHistogramMeta(?inRunId,
#'   'Stat G err: mean VS val: st. dev. \[counts\] \(EE')
#' ```
#' ```{sql connection = conn, output.var = "run2Ddata", echo = FALSE}
#' select * From getRun2DHistogramDataIdx(?inRunId,
#'   'Stat G err: mean VS val: st. dev. \[counts\] \(EE')
#' ```
#' ```{r hist2d_g_stdev_vs_meanerr, message=FALSE}
#' # plot.
#' create2DHistogram(run2Dmeta,run2Ddata)
#'
#' ```
#' }
create2DHistogram <- function(run2Dmeta,run2Ddata) {

  hc <- hchart(run2Ddata, type = "heatmap",
               hcaes(x = x, y = y, value = val)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_title(text = run2Dmeta$attribute) %>%
    hc_subtitle(text = "") %>%
    hc_exporting(enabled = TRUE) %>%

    #  hc_motion(enabled = TRUE, series = 0, startIndex = 0, labels = unique(df$year)) %>%
    hc_legend(layout = "vertical", verticalAlign = "top", align = "right") %>%
    hc_xAxis(
      title = list(text = run2Dmeta$xlabel),
      #type = ifelse( run2Dmeta$xscale == "LINEAR","", "logarithmic"),
      #x = seq(run2Dmeta$x_left,run2Dmeta$x_right, by=(run2Dmeta$x_right-run2Dmeta$x_left)/run2Dmeta$x_binsize * ),

      crosshair = TRUE) %>%
    hc_yAxis(
      title = list(text = run2Dmeta$ylabel),
      #type = ifelse( run2Dmeta$yscale == "LINEAR","", "logarithmic"),
      # y = run2Dmeta$y_left  + run2Ddata$y * (run2Dmeta$y_right - run2Dmeta$y_left) * run2Dmeta$y_binsize,
      #y = seq(run2Dmeta$y_left,run2Dmeta$y_right, by=run2Dmeta$y_binsize),
      crosshair = TRUE) %>%
    hc_colorAxis(minColor = "#0000FF", maxColor = "#FF0000",
                 type = "logarithmic") %>%
    hc_tooltip(pointFormat = "For bin [{point.x},{point.y}] there is <b>{point.value}</b> items<br/>", footerFormat = paste("All sources: <b> ", run2Dmeta$count , "</b><p>Failed sources: ",run2Dmeta$failed)) %>%
    hc_plotOptions(turboThreshold = 0)

  # knit_print_own.htmlwidget(hc, width="800px", height="600px")
  hc
}



#' Create a 2D histogram based on a Run data.
#'
#' Also out of a problematic (javascript-wise) data where we DO HAVE small fractional problem on the x,y axis.
#' @param run2Dmeta metadata about the plot
#' @param run2Ddata dataframe with the plot data
#' @param reverseY if to reverse Y scale, i.e. for Magnitude
#'
#' @return highcharts 2D histogram plot
#' @export
#'
#' @examples \dontrun{
#' # Get both metadata and data via two separate queries
#' ```{sql connection = conn, output.var = "run2Dmeta", echo = FALSE}
#' select * From getRun2DHistogramMeta(?inRunId,
#'   'Stat G err: mean VS val: st. dev. \[counts\] \(EE')
#' ```
#' ```{sql connection = conn, output.var = "run2Ddata", echo = FALSE}
#' select * From getRun2DHistogramDataIdx(?inRunId,
#'  'Stat G err: mean VS val: st. dev. \[counts\] \(EE')
#' ```
#' ```{r hist2d_g_stdev_vs_meanerr, message=FALSE}
#' # plot.
#' create2DHistogramFractions(run2Dmeta,run2Ddata)
#'
#' ```
#' }
create2DHistogramFractions <- function(run2Dmeta, run2Ddata, reverseY = FALSE) {

  xlogExpression = ifelse(run2Dmeta$xscale == "LINEAR","(","Math.pow(10,")
  ylogExpression = ifelse(run2Dmeta$yscale == "LINEAR","(","Math.pow(10,")
  hc <- hchart(run2Ddata, type = "heatmap",
               hcaes(x = x, y = y, value = val)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_title(text = run2Dmeta$attribute) %>%
    hc_subtitle(text = "") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_tooltip(formatter= JS(paste("function () { return 'For bin:<br/>",run2Dmeta$xlabel,":' + ",xlogExpression,run2Dmeta$x_left ,'+',  run2Dmeta$x_binsize,"* this.point.x) + '<br/>",run2Dmeta$ylabel,":' + ", ylogExpression,run2Dmeta$y_left ,'+',  run2Dmeta$y_binsize,"* this.point.y) +  '<br/>There is <b>' + this.point.value + '</b> items<br/>';}"))) %>%
    #  hc_motion(enabled = TRUE, series = 0, startIndex = 0, labels = unique(df$year)) %>%
    hc_legend(layout = "vertical", verticalAlign = "top", align = "right") %>%
    hc_xAxis(
      title = list(text = paste(ifelse( run2Dmeta$xscale == "LINEAR","", "log10 "),run2Dmeta$xlabel))
      # ,type = ifelse( run2Dmeta$xscale == "LINEAR","", "logarithmic"),
      ,categories = signif(seq(run2Dmeta$x_left-run2Dmeta$x_binsize, run2Dmeta$x_right+run2Dmeta$x_binsize, by = run2Dmeta$x_binsize  ),3)
      ,crosshair = TRUE

    ) %>%
    hc_yAxis(
      title = list(text = paste(ifelse( run2Dmeta$yscale == "LINEAR","", "log10 "),run2Dmeta$ylabel))
      #,type = ifelse( run2Dmeta$yscale == "LINEAR","", "logarithmic"),
      # ,y = run2Dmeta$y_left  + run2Ddata$y * (run2Dmeta$y_right - run2Dmeta$y_left) * run2Dmeta$y_binsize,
      ,categories = signif(seq(run2Dmeta$y_left-run2Dmeta$y_binsize, run2Dmeta$y_right+run2Dmeta$y_binsize, by = run2Dmeta$y_binsize  ),3)
      ,crosshair = TRUE
      ,reversed = reverseY
    )  %>%
    hc_colorAxis(minColor = "#0000FF", maxColor = "#FF0000", type = "logarithmic") %>%
    hc_plotOptions(turboThreshold = 0 )
  hc
}
