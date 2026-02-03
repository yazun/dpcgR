

#' dpcg ggplot2 default plot scheme
#'
#' @return simple scheme
#' @export
#' @importFrom ggplot2 theme_void element_text waiver
#' @importFrom ggplot2 ggplot aes ggtitle scale_x_reverse geom_point guide_legend guide_colourbar
#' @importFrom ggplot2 scale_colour_viridis_d scale_fill_viridis_d scale_shape_manual scale_colour_viridis_c scale_colour_gradient
#' @importFrom ggnewscale new_scale_colour
#' @importFrom ggpointdensity geom_pointdensity
#' @examples {
#' require(ggplot2)
#' theme = dpcgTheme()
#' }
dpcgTheme <- function() {
theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=30),
                            plot.subtitle = element_text(hjust = 0.5, size = 25),
                            plot.tag = element_text(hjust = 0.5),
                            axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 25),
                            axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 25)
                            )
theme
}



#' Background skymap plot generator
#'
#' Usually used to have a background and to overlay with \code{plotAitoffGalacticOverlay} or \code{plotAitoffGalacticOverlayBig} plots
#'
#' @param skyMapD dataframe with dataframe \code{(aitoffGl,aitoffGb)}
#' @param valueName Name of the \code{skymapD} dataframe field to be used as a colour
#' @param titleName Title of the plot
#' @param legendName - legend text
#' @param hpxLevel Healpix level (unused,possibly used for the legend)
#'
#' @return ggplot with Aitoff background
#' @export
#' @importFrom dplyr ensym
#' @importFrom ggplot2 theme theme_void element_text waiver
#' @importFrom ggplot2 ggplot aes ggtitle scale_x_reverse geom_point guide_legend guide_colourbar
#' @importFrom ggplot2 scale_colour_viridis_d scale_fill_viridis_d scale_shape_manual scale_colour_viridis_c scale_colour_gradient
#' @importFrom ggnewscale new_scale_colour
#' @importFrom scattermore geom_scattermore
#'
#' @examples \dontrun{
#' # fetch data via SQL chunk in at Healpix level 8'
#' ```{sql bkg_query, connection=conn, output.var="skyMapData.background", echo = TRUE, cache = TRUE}
#' select
#' coord[2]*180 / pi() alpha ,(pi()/2.0 - coord[1]) * 180.0/pi() delta
#' ,cnt_all
#' from (
#'   select converHealPixtoPositionLevel((hpx9::bit(64)>>( 2 * (9 - 8)))::bigint::int, 8::int) coord
#'   ,sum(cnt_g_gold) cnt_all
#'   from dr3_ops_cs34_mv.skymap_base_g_bp_rp_hpx9_2 group by 1
#' ) g;
#' ```
#'
#' # Then convert it to a
#'
#' skyMapFixed.background = skyMapData.background %>%
#'  mutate(alpha = ifelse(alpha>= 0, alpha, 360 + alpha))
#' skyMapGalactic.background
#'  = data.frame(
#'   skyMapFixed.background,
#'   togalactic(skyMapFixed.background$alpha, skyMapFixed.background$delta)) %>%
#'   mutate(aitoffGl = aitoffFn(gl,gb)$x, aitoffGb = aitoffFn(gl,gb)$y)
#'
#'# then save it to combine with overlay plots.
#'
#' bkg = plotAitoffGalacticBackground(skyMapGalactic.background,"cnt_all", "", "Counts", hpxLevel = 8)
#' }
plotAitoffGalacticBackground<-function(skyMapD, valueName, titleName, legendName, hpxLevel = 8 ) {

  skyMapD = skyMapD %>% mutate(alpha = ifelse(alpha>= 0, alpha, 360 + alpha))
  skyMapD = data.frame(skyMapD,togalactic(skyMapD$alpha,skyMapD$delta)) %>%
    mutate(aitoffGl = aitoffFn(gl,gb)$x, aitoffGb = aitoffFn(gl,gb)$y)

  valName = ensym(valueName)

  hpxDeg = dfHPX[dfHPX$Level==hpxLevel,"sq_deg"]

  theme <- theme_void() + theme(plot.title = element_text(hjust = 0.5, size=32),
                                plot.subtitle = element_text(hjust = 0.5),
                                plot.tag = element_text(hjust = 0.5)
  )

  ggplot(skyMapD,aes(x=aitoffGl,y=aitoffGb)  ) +
    ggtitle(titleName) +
    # geom_point(aes(colour = !!valName),alpha = .1, show.legend = FALSE) +
    geom_scattermore(aes(colour = !!valName),alpha = .3, show.legend = FALSE, pixels = c(1920,1080), pointsize = 3.2, interpolate = FALSE) +
    scale_x_reverse() +
    theme +
    # scale_colour_viridis_c(alpha= 0.2, option = "magma", trans="log10" , breaks = waiver()) +
    scale_colour_gradient(low = 'white', high = 'grey45', trans='log10', breaks = waiver())
}



#' Plot overlay distribution in a skyplot
#'
#'Uses geom_point for plotting.
#'
#' @param bkg background plot we overlay on
#' @param classGroup name of the primaryvartype to filter on (for classification.)
#' @param xm.skymap dataframe to plot, with alpha, delta, and primaryvartype fields
#'
#' @return ggplot skymap plot.
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 guides
#'
#' @examples \dontrun{
#' # brew chunks on the xm.groups for a massive markdown generation
#' brewed.chunks = brew_chunks(xm.groups_big,plotAitoffGalacticOverlay,xm.groups_small,xm.skymap)
#' }
plotAitoffGalacticOverlay <-function (bkg, classGroup, xm.skymap )
{
  classSet = unlist(classGroup)

  skyMapFixed.xm = xm.skymap %>%
    filter(primaryvartype %in% classSet) %>%
    mutate(alpha = ifelse(alpha>= 0, alpha, 360 + alpha))

  if(count(skyMapFixed.xm)==0) return(bkg + ggtitle(paste(classSet,collapse=" "), subtitle = "No objects."));

  skyMapGalactic.xm = data.frame(skyMapFixed.xm, togalactic(skyMapFixed.xm$alpha, skyMapFixed.xm$delta), skyMapFixed.xm$primaryvartype) %>%
    mutate(aitoffGl = aitoffFn(gl,gb)$x, aitoffGb = aitoffFn(gl,gb)$y)

  # plot all
  beginColor = .3
  bkg +
    ggtitle(paste(classSet,collapse=" "), subtitle = paste("[",length(skyMapGalactic.xm$primaryvartype),"] objects."))  +
    new_scale_colour()+
    geom_point(data = skyMapGalactic.xm, aes(x=aitoffGl,y=aitoffGb
                                             ,colour = factor(skyMapGalactic.xm$primaryvartype)
                                             ,shape = factor(skyMapGalactic.xm$primaryvartype)
                                             ,fill = factor(skyMapGalactic.xm$primaryvartype))
               , size = 1, alpha = .70, name = "All classes") +
    scale_colour_viridis_d(name = "Types", alpha= 0.6, option = "inferno", breaks = waiver(), labels = classSet, begin = beginColor, direction = -1) +
    scale_shape_manual(name = "Types", labels =  classSet, values = 1:length(classSet)) +
    scale_fill_viridis_d(name = "Types", alpha= 0.6, option = "inferno", breaks = waiver(), labels = classSet, begin = beginColor, direction = -1)
}


#' Plot overlay distribution in a skyplot
#'
#' Uses geom_scattermore for plotting.
#'
#' @param bkg background plot we overlay on
#' @param classGroup name of the primaryvartype to filter on (for classification.)
#' @param xm.skymap dataframe to plot, with alpha, delta, and primaryvartype fields
#' @param hpxLevel hpx level (for legend)
#'
#' @return ggplot skymap plot.
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 guides
#' @importFrom scattermore geom_scattermore
#'
#' @examples \dontrun{
#' # brew chunks on the xm.groups for a massive markdown generation
#' brewed.chunks = brew_chunks(xm.groups_big,plotAitoffGalacticOverlay,xm.groups_small,xm.skymap)
#' }
plotAitoffGalacticOverlayBig <-function (bkg, classGroup, xm.skymap, hpxLevel = 8 )
{
  classSet = unlist(classGroup)

  skyMapFixed.xm = xm.skymap %>%
    filter(primaryvartype %in% classSet) %>%
    mutate(alpha = ifelse(alpha>= 0, alpha, 360 + alpha))

  if(count(skyMapFixed.xm)==0) return(bkg + ggtitle(paste(classSet,collapse=" "), subtitle = "No objects."));

  skyMapGalactic.xm = data.frame(skyMapFixed.xm, togalactic(skyMapFixed.xm$alpha, skyMapFixed.xm$delta), skyMapFixed.xm$primaryvartype) %>%
    mutate(aitoffGl = aitoffFn(gl,gb)$x, aitoffGb = aitoffFn(gl,gb)$y)

  labelClass = paste(classSet,collapse=" ")
  hpxDeg = dfHPX[dfHPX$Level==hpxLevel,"sq_deg"]

  # plot all

  # move to non-yellow bands immediately if non-zero, but then the legend is broken
  beginColor = 0.0

  at.x =  outer(1:9, 10^(1:6))[1,]
  maxVal = max(xm.skymap[["cnt"]])
  lab.x <- c(ifelse(log10(at.x) %% 1 == 0, at.x, NA), maxVal)
  # my_breaks = ifelse(maxVal>=10^4, lab.x, waiver())
  my_breaks = lab.x

  bkg +
    ggtitle(labelClass, subtitle = paste("[",length(skyMapGalactic.xm$primaryvartype),"] objects."))  +

    new_scale_colour()+
    # geom_point(data = skyMapGalactic.xm, aes(x=aitoffGl,y=aitoffGb
    #                                          ,colour = cnt
    #                                          ,shape = factor(skyMapGalactic.xm$primaryvartype)
    #                                          # ,fill = factor(skyMapGalactic.xm$primaryvartype)
    #                                          )
    #            , size = .5, name = "Types") +
    geom_scattermore(
      data = skyMapGalactic.xm, aes(x=aitoffGl,y=aitoffGb
                                    ,colour = cnt
                                    ,shape = factor(skyMapGalactic.xm$primaryvartype)
                                    # ,fill = factor(skyMapGalactic.xm$primaryvartype)
      )
      , size = 1.6, name = "Types", alpha = .4
      , pixels = c(1920,1080), pointsize = 1.6, interpolate = FALSE) +
    # scale_colour_viridis_c(name = "Types", alpha= 0.6, option = "inferno", breaks = waiver(), labels = classSet, begin = beginColor, direction = -1) +
    scale_shape_manual(name = "Types", labels =  classSet, values = 1:length(classSet)) +
    #scale_fill_viridis_c(name = "Types", alpha= 0.6, option = "inferno", breaks = waiver(), labels = classSet, begin = beginColor, direction = -1) +
    scale_colour_viridis_c(name = "Density",  alpha = 0.4, option = "magma", trans="log10" , breaks = waiver()) +
    guides(colour = guide_colourbar(
      title = bquote(.("Sources") ~ " per " ~ (.(hpxDeg) ~ Deg^2)  ), barwidth = 1, barheight = 20, title.position = "bottom", order = 2, title.hjust = 0
      ,title.theme = element_text(size = 15,angle = 0)
      ,breaks = my_breaks, values = my_breaks
    ),
    shape = guide_legend(order = 1, title.hjust = .5)
    )
}


#' Plot overlay distribution in a skyplot with a single class attribute
#'
#' Uses geom_scattermore for plotting.
#'
#' @param bkg background plot we overlay on
#' @param className name of the primaryvartype to filter on (for classification.)
#' @param xm.skymap dataframe to plot, with alpha, delta fields
#' @param hpxLevel expected hpxLevel of the input.
#' @param alpha name of alpha in Deg in df
#' @param delta name of delta in Deg in df
#' @param palette name of the viridis palette to use for the type.
#' @param adjuster weight for density smoothing by geom_pointdensity
#'
#' @return ggplot skymap plot overlayd over background plot
#' @export
#' @importFrom dplyr mutate
#' @importFrom ggplot2 guides
#'
#' @examples \dontrun{
#' # brew chunks on the xm.groups for a massive markdown generation
#' brewed.chunks =
#'   brew_chunks(xm.groups_big
#'   ,plotAitoffGalacticOverlayBigSingleType
#'    , bkg
#'    , sosType
#'    , sosConfigName)
#' }

plotAitoffGalacticOverlayBigSingleType<-function (bkg, className, xm.skymap, alpha = "ra_deg", delta = "dec_deg", hpxLevel = 8, palette = "plasma", adjuster = 6 )
{
  skyMapFixed.xm = xm.skymap %>%
    # filter(primaryvartype %in% classSet) %>%
    mutate(alpha = ifelse(!!as.name(alpha)>= 0, !!as.name(alpha), 360 + !!as.name(alpha)))

  if(count(skyMapFixed.xm)==0) return(bkg + ggtitle(paste(className,collapse=" "), subtitle = "No objects."));

  labelClass = paste(className,collapse=" ")
  hpxDeg = dfHPX[dfHPX$Level==hpxLevel,"sq_deg"]

  skyMapGalactic.xm = data.frame(skyMapFixed.xm, togalactic(skyMapFixed.xm[[alpha]], skyMapFixed.xm[[delta]])) %>%
    mutate(aitoffG = aitoffFn(gl,gb), originalCnt = cnt, cnt = as.numeric(cnt) * hpxDeg)
  # plot all
  # move to non-yellow bands immediately if non-zero, but then the legend is broken
  beginColor = 0.0

  adjusting = case_when(nrow(skyMapGalactic.xm ) < 50000 ~ 0.1, TRUE ~ adjuster)
  sizer = case_when(nrow(skyMapGalactic.xm ) < 1000 ~ 3,
                    nrow(skyMapGalactic.xm ) < 20000 ~ 1,
                    nrow(skyMapGalactic.xm ) < 50000 ~ .8,
                    nrow(skyMapGalactic.xm ) < 100000 ~ .4,
                    TRUE ~ .1)

  transer = case_when(max(skyMapGalactic.xm$cnt ) < 1000 ~ list(fnName = "identity",fn = identity),
                      TRUE ~ list(fnName = "log10",fn = function(x) ifelse(log10(x) %% 1 == 0, x, NA)))

  at.x =  outer(1:9, 10^(1:6))[1,]
  maxVal = max(xm.skymap[["cnt"]])
  lab.x <- c(transer[[2]](at.x) , maxVal)

  # my_breaks = ifelse(maxVal>=10^4, lab.x, waiver())
  my_breaks = lab.x

  bkg +
    ggtitle(labelClass, subtitle = paste("[",sum(skyMapGalactic.xm$originalCnt),"] objects."))  +
    new_scale_colour() +
    geom_scattermore(
      data = skyMapGalactic.xm, aes(x=aitoffG$x,y=aitoffG$y
                                    ,colour = cnt
                                    ,shape = factor(className)
                                    # ,fill = factor(skyMapGalactic.xm$primaryvartype)

      )
      , size = 1.6
      , name = "Types", alpha = .4
      , pixels = c(1920,1080), pointsize = 1.6, interpolate = FALSE) +
    # scale_colour_viridis_c(name = "Types", alpha= 0.6, option = "inferno", breaks = waiver(), labels = classSet, begin = beginColor, direction = -1) +
    scale_shape_manual(name = "Type", labels =  className, values = 1:length(className)) +
    #scale_fill_viridis_c(name = "Types", alpha= 0.6, option = "inferno", breaks = waiver(), labels = classSet, begin = beginColor, direction = -1) +
    scale_colour_viridis_c(name = "identity",  alpha = 0.5, option = palette, trans=transer[[1]] , breaks = waiver()) +

    # geom_pointdensity(data= skyMapGalactic.xm, aes(x=aitoffG$x,y=aitoffG$y), shape=16, alpha = .5, size = sizer,
    #                   adjust = adjuster,
    #                   show.legend = TRUE
    # ) +
    scale_colour_viridis_c(option = palette) +
    guides(colour = guide_colourbar(
      title = bquote(.("Sources") ~ "per"~deg^2), barwidth = 1, barheight = 20, title.position = "bottom", order = 2, title.hjust = 0
      ,title.theme = element_text(size = 15, angle = 0)
      ,breaks = my_breaks, values = my_breaks
    ),
    shape = guide_legend(order = 1, title.hjust = .5)
    )

}

#' Plots CMD and HR diagrams in a combo
#'
#' @param inData dataframe with CMD/HR data
#' @param valueName name of the plots
#' @param catalogName unused for now. We could show literature ones here.
#' @param palette viridis palette name for density-colour of the points
#' @param varpi_over_varpierror_cut cut for HR diagram
#' @param adjuster factor to adjust smoothing for density kernel
#' @param data.bkg.hr data for Hr diagram
#' @param data.bkg.cmd data for cmd diagram
#'
#' @return two-plot ggplot with CMD and HR
#' @export
#' @importFrom dplyr mutate filter
#' @importFrom ggplot2 guides theme
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpointdensity geom_pointdensity
#'
#' @examples \dontrun{
#' plotCmdAndHR(sosSet, valueName = cu7Name, catalogName = NULL)
#' }
#'
plotCmdAndHR <- function(inData, valueName, catalogName = NULL, palette = "plasma",  varpi_over_varpierror_cut = 5, adjuster = 5, data.bkg.hr = data.bkg.hr, data.bkg.cmd = data.bkg.cmd){

  # if(!is.null(catalogName)){
  #     wantedData <- data[which(data$type %in% typeid & data$catalog==catalogName),]
  # }else{
  #     wantedData <- data[data$type %in% typeid,]
  # }
  wantedData = inData
  title = paste(valueName)

  wantedData$median_bp_minus_median_rp <- (wantedData$fmedian_bp - wantedData$fmedian_rp)

  #ABSOLUTE COLOR MAGNITUDE
  wantedData = wantedData %>%
    mutate (median_bp_minus_median_rp = fmedian_bp - fmedian_rp)
  # filter (!is.na(wantedData$median_bp_minus_median_rp) && !is.na(varpi_mas) && !is.na(fmedian_g)) %>%
  ;

  wantedHR = wantedData %>% filter( varpi_mas/varpierror_mas > varpi_over_varpierror_cut)  %>%
    mutate( median_g_abs = fmedian_g + 5 + 5*log10(varpi_mas/1000));

  # pCM = plot(wantedData$median_bp_minus_median_rp, wantedData$g_median, main=paste("Color Magnitude for", title), pch=20, col=rgb(32,39,247,90,maxColorValue=255), xlab="median BP - median RP", ylab="Median G")
  theme <- theme_bw() + ggplot2::theme(plot.title = element_text(hjust = 0.5, size=30),
                              plot.subtitle = element_text(hjust = 0.5, size = 25),
                              plot.tag = element_text(hjust = 0.5),
                              axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 25),
                              axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 25)
  )

  adjusting = case_when(nrow(wantedData) < 50000 ~ 0.1, TRUE ~ adjuster)
  sizer = case_when(nrow(wantedData ) < 20000 ~ 1.5,
                    nrow(wantedData ) < 50000 ~ 1,
                    nrow(wantedData ) < 100000 ~ .75,
                    TRUE ~ .5)


  pCM = ggplot() +
    ggtitle(paste("Color Magnitude for", title), subtitle = paste(nrow(inData),"sources"))  +
    # geom_tile(data = data.bkg.cmd, aes( x= median_bp_minus_median_rp, y = fmedian_g, fill = (cnt))) +
    geom_tile(data = data.bkg.cmd, aes(median_bp_minus_median_rp, fmedian_g, fill = log(cnt)), alpha = 1
              ,  width = .01, height = .01, show.legend = FALSE
    ) +

    # scale_fill_continuous(low="lightgrey", high="black") +
    scale_fill_gradient(low="lightgrey", high="black") +
    # geom_point(data = wantedData, aes(x = median_bp_minus_median_rp, y =  fmedian_g), shape=16, alpha = .5, colour = pcolour) +
    geom_pointdensity(data = wantedData, aes(x = median_bp_minus_median_rp, y =  fmedian_g), shape=16, alpha = .5, size = sizer,
                      adjust = adjusting,
                      show.legend = FALSE) +
    scale_colour_viridis_c(option = palette) +
    # stat_density_2d(data = wantedData, aes(x = median_bp_minus_median_rp, y =  fmedian_g, fill = ..density..), geom = "raster", contour = FALSE) +
    labs(x = "median BP - median RP", y = "Median G")  +
    theme +
    scale_y_reverse()

  pAM = ggplot() +
    ggtitle(paste("Color Absolute Magnitude for", title), subtitle = paste(nrow(wantedHR),"sources after parallax cut"))  +
    geom_tile(data = data.bkg.hr, aes(x = median_bp_minus_median_rp, y = median_g_abs, fill = log(cnt)), alpha = 1
              ,  width = .01, height = .01, show.legend = FALSE
    ) +
    scale_fill_continuous(low="lightgrey", high="black") +
    # geom_point(data = wantedHR, aes(x = median_bp_minus_median_rp, y =  median_g_abs),shape=16, alpha = .5, colour = pcolour) +
    geom_pointdensity(data = wantedHR, aes(x = median_bp_minus_median_rp, y =  median_g_abs), shape=16, alpha = .5,size = sizer,
                      adjust = adjusting,
                      show.legend = FALSE) +
    scale_colour_viridis_c(option = palette) +

    labs(x="median BP - median RP", y="Absolute Median G")  +
    theme +
    scale_y_reverse()

  # pAM = plot(abs_mag_data$median_bp_minus_median_rp, abs_mag_data$median_g_abs, main=paste("Color Absolute Magnitude for", title), pch=20, col=rgb(32,39,247,90,maxColorValue=255), xlab="Median BP - Median RP", ylab="Absolute Median G")
  # lay <- rbind(
  #   c(1,1)
  #   ,c(1,2)
  # )
  grid.arrange(pCM, pAM, nrow = 1 )

}



#' Create area hisgoram with cumulatvie sum
#'
#' @param histData dataframe with x,val
#' @param histMetaData dataframe with all needed fields: aribute, xlabel , scale = "LINEAR"/"LOG", failed, count for sidplauy purposes
#' @param plotType type of the plot (scatter,area, column, ...)
#' @param xaxisType put "logarithmic" is needed for X-axis
#'
#' @return highcharter area histogram
#' @export
#'
#' @examples \dontrun{
#' periodData = sosSet %>%
#' group_by(x = round(period,2)) %>%
#' summarise(val = n()) %>% ungroup()
#' create1DHistogramRaw(periodData,
#'  metaData =
#'   data.frame(attribute = "Period",
#'   xlabel = paste(cu7Name, "Period"),
#'   scale = "LINEAR",
#'   failed = NaN,
#'   count = length(sosSet[[1]])))
#'
#' }
create1DHistogramRaw <- function(histData, histMetaData, plotType = "area", xaxisType = "") {

  if(length(which (histData$val != 0 ))!=0) {
    histData<-histData[(min(which ( histData$val != 0 ))-1) : (max( which( histData$val != 0 ))+1),]
  }
  cumV = cumsum(histData$val)
  entryRow <- histMetaData[1,]
  seriesName = entryRow[,"attribute"]

  hc <- highchart(type = "chart") %>%
    hc_title(text = paste(seriesName, "histogram")) %>%
    hc_subtitle(text = "") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_chart(zoomType = "x") %>%
    hc_xAxis(
      type = xaxisType,
      title = list(text = entryRow$xlabel),
      x = histData$x,
      crosshair = TRUE,
      plotLines = list(
        list(label = list(text = "median"),
             color = "#FF0000",
             width = .3,
             value = histData$x[which.min(abs(cumV/max(cumV)-0.5))]),
        list(label = list(text = "mean"),
             color = "#FF0000",
             width = .3,
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
      list(nid = 3, type = ifelse( entryRow$scale == "LINEAR","", "logarithmic"), title = list(text = "Count"), crosshair = TRUE),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Cumulative"))
    ) %>%
    hc_tooltip(pointFormat = "For bin {point.x} there is <b>{point.y}</b> items<br/>", footerFormat = paste("All sources: <b> ", entryRow$count , "</b><p>Failed sources: ",entryRow$failed))  %>%
    hc_add_series(histData, hcaes( y = val, name = !!seriesName  ), name = seriesName,  id = "series", type = plotType, yAxis = 0)  %>%
    hc_add_series(histData, hcaes(name = paste("Cumulative",!!seriesName) , y =  cumsum(val)) , name =  paste("Cumulative",seriesName), id = "Cumulative",  type = "scatter" , yAxis = 1)
  hc
}

#' Create faceted plot based on a timeseries data and period, if exists.
#' @param ts.all timeseries data to plot
#' @param sosSet dataframe with period
#'
#' @return faceted plot with timeseries and folded timeseries if period exists
#' @export
#'
#' @examples \dontrun{
#' plotTs(ts.all, sosSet)
#' }
plotTs<-function (ts.all, sosSet) {

  theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=14),
                              plot.subtitle = element_text(hjust = 0.5, size = 13),
                              plot.tag = element_text(hjust = 0.5),
                              axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10),
                              axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10)
                              ,legend.position="bottom", legend.direction="vertical"
  )

  derived = ggplot() +
    ggtitle(paste("Timeseries ", ""), subtitle = "")  +
    geom_pointrange(data = ts.all, mapping = aes(x = obstime,y= val, ymin=val-valerr, ymax=val+valerr, color = tag), size = .05) +
    labs(x = "JD", y = "Magnitude")  +
    theme +
    scale_y_reverse() + facet_wrap(~sourceid, ncol = 1, scales="free_y") +
    scale_color_manual(values=c("blue", "darkgrey", "red"))

    # get period
  if ("period" %in% colnames(sosSet)){
    f = ts.all %>% mutate (sourceid = as.integer64(sourceid)) %>% inner_join(sosSet, by ="sourceid")

    #wrapper for dplyr
    get_folds <- function(fd) {
      d = foldTimeseriesFull (fd$sourceid, fd$tag, fd$period, fd$obstime, fd$val, fd$valerr,mean(fd$obstime))
      return(d);
    }

    g = f %>% filter (!is.na(period)) %>% group_by(sourceid, tag) %>%
      do(get_folds(.))

    folded = ggplot() +
      ggtitle(paste("Folded timeseries ", ""), subtitle = "")  +
      geom_pointrange(data = g , mapping = aes(x = phases,y= magnitudes, ymin=magnitudes-errors, ymax=magnitudes+errors, color = tag), size = .05) +
      labs(x = "JD", y = "Magnitude")  +
      theme +
      scale_y_reverse() +
      facet_wrap( ~ sourceid, ncol = 1, scales="free_y") +
      scale_color_manual(values=c("blue", "darkgrey", "red")) +
      geom_text(data  = sosSetSample[c("sourceid","period")] %>% mutate(sourceid = as.character(sourceid)), mapping = aes(x = Inf, y = Inf, label=paste("Period:",round(period,4))), size=3, hjust = 1.1, vjust= -2, parse=FALSE)

    return (grid.arrange(derived, folded, nrow = 1 ))
  }
  return (grid.arrange(derived, nrow = 1 ))
}



#' Plots CMD and HR diagrams in a combo using two, precomputed HR and CMD sets.
#' I.e. From sql queries.
#'
#' @param inDataCmd dataframe with CMD data
#' @param inData dataframe with HR data
#' @param valueName name of the plots
#' @param catalogName unused for now. We could show literature ones here.
#' @param palette viridis palette name for density-colour of the points
#' @param data.bkg.hr data for Hr diagram
#' @param data.bkg.cmd data for cmd diagram
#'
#' @return two-plot ggplot with CMD and HR
#' @export
#' @importFrom dplyr mutate filter
#' @importFrom ggplot2 guides theme
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpointdensity geom_pointdensity
#'
#' @examples \dontrun{
#' plotCmdAndHRSeparateSets(sosSetCmd,sosSetHr, valueName = cu7Name, catalogName = NULL)
#' }
#'
plotCmdAndHRSeparateSets<-function (inDataCmd, inData, valueName, catalogName = NULL, palette = "plasma",data.bkg.hr = data.bkg.hr,data.bkg.cmd = data.bkg.cmd)
{
  wantedData = inData
  wantedDataCmd = inDataCmd
  title = paste(valueName)

  theme <- theme_bw() + ggplot2::theme(plot.title = element_text(hjust = 0.5,size = 30),
                                       plot.subtitle = element_text(hjust = 0.5,size = 25),
                                       plot.tag = element_text(hjust = 0.5), axis.text.x = element_text(size = 20),
                                       axis.text.y = element_text(size = 25), axis.title.x = element_text(size = 20),
                                       axis.title.y = element_text(size = 25))

  sizer = case_when(nrow(wantedData) < 20000 ~ 1.5, nrow(wantedData) <
                      50000 ~ 1, nrow(wantedData) < 100000 ~ 0.75, TRUE ~ 0.5)

  pCM = ggplot() + ggtitle(paste("Color Magnitude for", title),
                           subtitle = paste(nrow(wantedDataCmd), "sources")) +
    geom_tile(data = data.bkg.cmd, aes(median_bp_minus_median_rp, fmedian_g, fill = log(cnt)),
     alpha = 1, width = 0.01, height = 0.01, show.legend = FALSE) +
    scale_fill_gradient(low = "lightgrey", high = "black") +
    geom_point(data = wantedDataCmd, aes(x = median_bp_minus_median_rp, y = median_g, colour = log(cnt)), shape = 16, alpha = 0.5, size = sizer, show.legend = FALSE) +
    scale_colour_viridis_c(option = palette) +
    labs(x = "median BP - median RP", y = "Median G") + theme +
    scale_y_reverse()

  pAM = ggplot() + ggtitle(paste("Color Absolute Magnitude for",
                                 title), subtitle = paste(nrow(wantedHR), "sources after the parallax cut")) +
    geom_tile(data = data.bkg.hr, aes(x = median_bp_minus_median_rp,
                                      y = median_g_abs, fill = log(cnt)), alpha = 1, width = 0.01,
              height = 0.01, show.legend = FALSE) +
    scale_fill_continuous(low = "lightgrey",high = "black") +
    geom_point(data = wantedData,
               aes(x = median_bp_minus_median_rp, y = median_g_abs, colour = log(cnt) ),
               shape = 16, alpha = 0.5, size = sizer,
               show.legend = FALSE) + scale_colour_viridis_c(option = palette) +
    labs(x = "median BP - median RP", y = "Absolute Median G") +
    theme + scale_y_reverse()

    return(grid.arrange(pCM, pAM, nrow = 1))
}



#' Extract Histogram Data Structures from Analysis Results
#'
#' Processes combined histogram data from SOS analysis results and organizes it
#' into two separate data structures: per-classifier histograms and merged histograms.
#' This function is typically called internally by analysis pipelines to prepare
#' data for visualization.
#'
#' @param results A list containing analysis results with the following components:
#'   \describe{
#'     \item{metadata}{A list with \code{sosname} (character) identifying the SOS type}
#'     \item{histogram_combined}{A data frame with histogram data containing columns:
#'       \code{label}, \code{cut_type}, \code{classifierid}, \code{bucket}, \code{freq},
#'       \code{training_freq}, \code{bucket_start}, \code{bucket_end}, \code{bucket_mid}}
#'   }
#'
#' @return A list with two components:
#'   \describe{
#'     \item{histograms_per_classifier}{A nested list organized by SOS name, then by
#'       cut type (\code{with_cuts}, \code{classifier_only}), then by classifier name.
#'       Each leaf contains a filtered data frame for that specific classifier.}
#'     \item{histograms_merged}{A nested list organized by SOS name, then by cut type.
#'       Contains aggregated data across all classifiers with labels concatenated.}
#'   }
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Separates data by cut type (with cuts vs. classifier only)
#'   \item Extracts unique classifier names from labels (format: "classifier:type")
#'   \item Creates per-classifier subsets for each cut type
#'   \item Aggregates "all_conditions" data into merged histograms with combined labels
#' }
#'
#' Labels in the input data follow the format "classifier_name:type_name" (e.g.,
#' "DSCTF:RRab", "Nss:LPV"). The special label "all_conditions" represents merged
#' data across all classifier conditions.
#'
#' @examples
#' \dontrun{
#' # After running SOS analysis
#' results <- analyze_sos_histograms(sosname = "RR_LYRAE", ...)
#'
#' # Extract structured histogram data
#' hist_data <- extract_histogram_data(results)
#'
#' # Access per-classifier data
#' dsctf_with_cuts <- hist_data$histograms_per_classifier$RR_LYRAE$with_cuts$DSCTF
#'
#' # Access merged data
#' merged_no_cuts <- hist_data$histograms_merged$RR_LYRAE$classifier_only
#' }
#'
#' @seealso \code{\link{plot_histograms_from_results}} for visualizing the extracted data,
#'   \code{\link{plot_histogram_from_dataframe}} for direct plotting
#'
#' @export
extract_histogram_data <- function(results) {


  sosname <- results$metadata$sosname
  histogram_combined <- results$histogram_combined

  # Separate data by cut type for downstream processing
  data_with_cuts <- histogram_combined %>%
    filter(cut_type == "with_cut")

  data_no_cuts <- histogram_combined %>%
    filter(cut_type == "no_cut")

  # Initialize nested list structures for organized storage
  histograms_per_classifier <- list()
  histograms_per_classifier[[sosname]] <- list(
    with_cuts = list(),
    classifier_only = list()
  )

  histograms_merged <- list()
  histograms_merged[[sosname]] <- list(
    with_cuts = NULL,
    classifier_only = NULL
  )

  # Get unique classifiers (excluding the aggregated "all_conditions" label)
  classifiers <- unique(histogram_combined$label[histogram_combined$label != "all_conditions"])


  # Extract classifier names from labels (format: "classifier:type")
  # e.g., "DSCTF:RRab" -> "DSCTF"
  classifier_names <- unique(sapply(strsplit(classifiers, ":"), `[`, 1))

  # Process per-classifier data: iterate through each classifier
  # and extract its specific histogram data for both cut types
  for (classifier_name in classifier_names) {
    # Build regex pattern to match all types for this classifier
    # e.g., "^DSCTF:" matches "DSCTF:RRab", "DSCTF:RRc", etc.
    classifier_types <- classifiers[grepl(paste0("^", classifier_name, ":"), classifiers)]

    # Extract with-cuts data for this classifier
    classifier_data_with_cuts <- data_with_cuts %>%
      filter(label %in% classifier_types) %>%
      select(label, classifierid, bucket, freq, training_freq, bucket_start, bucket_end, bucket_mid)

    if (nrow(classifier_data_with_cuts) > 0) {
      histograms_per_classifier[[sosname]]$with_cuts[[classifier_name]] <- classifier_data_with_cuts
    }

    # Extract classifier-only (no cuts) data for this classifier
    classifier_data_no_cuts <- data_no_cuts %>%
      filter(label %in% classifier_types) %>%
      select(label, classifierid, bucket, freq, training_freq, bucket_start, bucket_end, bucket_mid)

    if (nrow(classifier_data_no_cuts) > 0) {
      histograms_per_classifier[[sosname]]$classifier_only[[classifier_name]] <- classifier_data_no_cuts
    }
  }

  # Process merged data: extract "all_conditions" aggregated histograms
  # and relabel with concatenated classifier names for reference
  merged_with_cuts <- data_with_cuts %>%
    filter(label == "all_conditions") %>%
    rename(lbl = label) %>%
    # Create descriptive label showing all contributing classifiers
    mutate(lbl = paste(sort(unique(classifiers)), collapse = ",")) %>%
    select(lbl, classifierid, bucket, freq, training_freq, bucket_start, bucket_end, bucket_mid)

  if (nrow(merged_with_cuts) > 0) {
    histograms_merged[[sosname]]$with_cuts <- merged_with_cuts
  }

  merged_no_cuts <- data_no_cuts %>%
    filter(label == "all_conditions") %>%
    rename(lbl = label) %>%
    mutate(lbl = paste(sort(unique(classifiers)), collapse = ",")) %>%
    select(lbl, classifierid, bucket, freq, training_freq, bucket_start, bucket_end, bucket_mid)

  if (nrow(merged_no_cuts) > 0) {
    histograms_merged[[sosname]]$classifier_only <- merged_no_cuts
  }

  return(list(
    histograms_per_classifier = histograms_per_classifier,
    histograms_merged = histograms_merged
  ))
}


#' Plot All Histograms from SOS Analysis Results
#'
#' Generates a complete set of interactive histogram plots from SOS analysis results,
#' including per-classifier plots and merged plots for both cut types (with cuts and
#' without cuts). This is a high-level wrapper function that automates the full
#' visualization workflow.
#'
#' @param results A list containing analysis results with the following components:
#'   \describe{
#'     \item{metadata}{A list containing \code{sosname} (character) and \code{cuts}
#'       (nested list of cut values by classifier and type)}
#'     \item{histogram_combined}{A data frame with histogram data}
#'   }
#' @param output_dir Character string specifying the directory for saving HTML plots.
#'   Default is \code{"histogram_plots"}. Set to \code{NULL} to skip saving.
#' @param show_plots Logical indicating whether to display plots interactively.
#'   Default is \code{TRUE}. Set to \code{FALSE} for batch processing.
#' @param show_training Logical indicating whether to include training data
#'   visualization (green bars and cumulative sum lines). Default is \code{TRUE}.
#'
#' @return Invisibly returns a named list of all generated plotly objects.
#'   Plot names follow the pattern: \code{"{sosname}_{classifier}_{cut_type}"}
#'   for per-classifier plots, and \code{"{sosname}_merged_{cut_type}"} for
#'   merged plots.
#'
#' @details
#' For each SOS type in the results, this function generates:
#' \itemize{
#'   \item Per-classifier histograms with cuts applied
#'   \item Per-classifier histograms without cuts (classifier-only)
#'   \item Merged histogram with cuts (all classifiers combined)
#'   \item Merged histogram without cuts
#' }
#'
#' Each plot includes:
#' \itemize{
#'   \item Bar chart of frequency distribution by posterior probability
#'   \item Training data overlay (optional, in green)
#'   \item Inverse cumulative sum line on secondary y-axis
#'   \item Cut threshold lines (for with-cuts plots)
#' }
#'
#' Progress messages are printed to the console during execution.
#'
#' @examples
#' \dontrun{
#' # Generate all plots and display interactively
#' results <- analyze_sos_histograms(sosname = "RR_LYRAE", ...)
#' all_plots <- plot_histograms_from_results(results)
#'
#' # Batch processing: save plots without displaying
#' plot_histograms_from_results(
#'   results,
#'   output_dir = "output/histograms",
#'   show_plots = FALSE
#' )
#'
#' # Access a specific plot from the returned list
#' all_plots$RR_LYRAE_DSCTF_with_cut
#' }
#'
#' @seealso \code{\link{plot_histogram_from_dataframe}} for plotting individual histograms,
#'   \code{\link{extract_histogram_data}} for extracting structured data
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr filter
#' @importFrom htmlwidgets saveWidget
#'
#' @export
plot_histograms_from_results <- function(results,
                                         output_dir = "histogram_plots",
                                         show_plots = TRUE,
                                         show_training = TRUE) {

  # Extract metadata from results
  sosname <- results$metadata$sosname
  cuts_info <- results$metadata$cuts
  histogram_combined <- results$histogram_combined

  # Identify unique classifiers from the data labels
  # Exclude "all_conditions" which represents merged data
  all_labels <- unique(histogram_combined$label)
  all_labels <- all_labels[all_labels != "all_conditions"]

  # Extract unique classifier names (portion before ":")
  classifier_names <- unique(sapply(strsplit(all_labels, ":"), `[`, 1))

  # Container for all generated plots
  all_plots <- list()

  cat(sprintf("\n=== Plotting histograms for %s ===\n", sosname))

  # Generate per-classifier histograms for each classifier
  for (classifier_name in classifier_names) {
    # Plot with cuts applied
    cat(sprintf("  Plotting %s - with cuts\n", classifier_name))

    p1 <- plot_histogram_from_dataframe(
      histogram_combined = histogram_combined,
      sosname = sosname,
      cuts_info = list(sosname = cuts_info),
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

    # Plot without cuts (classifier-only)
    cat(sprintf("  Plotting %s - no cuts\n", classifier_name))

    p2 <- plot_histogram_from_dataframe(
      histogram_combined = histogram_combined,
      sosname = sosname,
      cuts_info = list(sosname = cuts_info),
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

  # Generate merged histograms (all classifiers combined)
  cat(sprintf("  Plotting merged - with cuts\n"))

  p3 <- plot_histogram_from_dataframe(
    histogram_combined = histogram_combined,
    sosname = sosname,
    cuts_info = list(sosname = cuts_info),
    classifier_name = NULL,  # NULL indicates merged plot
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

  p4 <- plot_histogram_from_dataframe(
    histogram_combined = histogram_combined,
    sosname = sosname,
    cuts_info = list(sosname = cuts_info),
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


#' Create Interactive Histogram Plot from Data Frame
#'
#' Generates an interactive plotly histogram visualization from pre-processed
#' histogram data. Supports both per-classifier and merged views, with optional
#' training data overlay, inverse cumulative sum curves, and cut threshold lines.
#'
#' @param histogram_combined A data frame containing histogram data with columns:
#'   \describe{
#'     \item{label}{Character identifying the classifier:type (e.g., "DSCTF:RRab")
#'       or "all_conditions" for merged data}
#'     \item{cut_type}{Character: "with_cut" or "no_cut"}
#'     \item{bucket}{Integer bucket index}
#'     \item{bucket_start}{Numeric lower bound of probability bucket}
#'     \item{bucket_end}{Numeric upper bound of probability bucket}
#'     \item{bucket_mid}{Numeric midpoint of probability bucket (used for x-axis)}
#'     \item{freq}{Integer frequency count for this bucket}
#'     \item{training_freq}{Integer frequency count from training sources}
#'   }
#' @param sosname Character string identifying the SOS type (e.g., "RR_LYRAE").
#'   Used for plot titles and file naming.
#' @param cuts_info Nested list containing cut threshold values. Structure:
#'   \code{cuts_info[[sosname]][[classifier_name]][[type_label]]} returns the
#'   numeric cut value for that specific type.
#' @param classifier_name Character string specifying which classifier to plot.
#'   Set to \code{NULL} for merged (all classifiers) plot. Default is \code{NULL}.
#' @param cut_type Character string: \code{"with_cut"} or \code{"no_cut"}.
#'   Determines which subset of data to visualize. Default is \code{"with_cut"}.
#' @param show_cumsum Logical indicating whether to display inverse cumulative
#'   sum lines on secondary y-axis. Default is \code{TRUE}.
#' @param show_cuts Logical indicating whether to display vertical cut threshold
#'   lines. Default is \code{TRUE}.
#' @param show_training Logical indicating whether to display training data
#'   (green bars and cumulative sum). Default is \code{TRUE}.
#' @param output_dir Character string specifying directory for saving HTML output.
#'   Set to \code{NULL} to skip saving. Default is \code{NULL}.
#'
#' @return A plotly object containing the interactive histogram, or \code{NULL}
#'   if no data is available for the specified parameters.
#'
#' @details
#' The function creates a multi-layer plotly visualization:
#'
#' \strong{For merged plots} (\code{classifier_name = NULL}):
#' \itemize{
#'   \item Data is aggregated across all classifier types
#'   \item Single bar series in steelblue
#'   \item Training data shown as green bars with reduced opacity
#' }
#'
#' \strong{For per-classifier plots}:
#' \itemize{
#'   \item Separate bar series for each type within the classifier
#'   \item Colors cycle through a 10-color palette
#'   \item Legend groups bars and lines by type
#' }
#'
#' \strong{Inverse Cumulative Sum}:
#' The inverse cumsum represents the number (or proportion) of sources with
#' probability >= the current bucket's midpoint. This is calculated as:
#' \code{total - forward_cumsum + current_freq}
#'
#' \strong{Automatic Log Scale}:
#' Log scale is automatically applied to the y-axis when the frequency range
#' spans more than 5x (i.e., max/min > 5).
#'
#' \strong{Hover Information}:
#' Each bar displays: probability value, frequency, training count,
#' inverse cumulative sum, and percentage.
#'
#' @examples
#' \dontrun{
#' # Plot a specific classifier with cuts
#' fig <- plot_histogram_from_dataframe(
#'   histogram_combined = results$histogram_combined,
#'   sosname = "RR_LYRAE",
#'   cuts_info = list(RR_LYRAE = results$metadata$cuts),
#'   classifier_name = "DSCTF",
#'   cut_type = "with_cut"
#' )
#' fig
#'
#' # Plot merged histogram without training data
#' fig_merged <- plot_histogram_from_dataframe(
#'   histogram_combined = results$histogram_combined,
#'   sosname = "RR_LYRAE",
#'   cuts_info = list(RR_LYRAE = results$metadata$cuts),
#'   classifier_name = NULL,
#'   show_training = FALSE,
#'   output_dir = "plots"
#' )
#' }
#'
#' @seealso \code{\link{plot_histograms_from_results}} for batch plotting,
#'   \code{\link{extract_histogram_data}} for data extraction
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr filter group_by summarise arrange mutate ungroup
#' @importFrom htmlwidgets saveWidget
#'
#' @export
plot_histogram_from_dataframe <- function(histogram_combined,
                                          sosname,
                                          cuts_info,
                                          classifier_name = NULL,
                                          cut_type = "with_cut",
                                          show_cumsum = TRUE,
                                          show_cuts = TRUE,
                                          show_training = TRUE,
                                          output_dir = NULL) {

  library(plotly)
  library(dplyr)

  # Determine if this is a merged (all classifiers) or single-classifier plot
  is_merged <- is.null(classifier_name)

  # Filter data based on plot type (merged vs. per-classifier)
  if (is_merged) {
    # Merged plot: use aggregated "all_conditions" data
    data <- histogram_combined %>%
      filter(label == "all_conditions", cut_type == !!cut_type)
    title <- sprintf("%s - Merged Histogram (%s)", sosname,
                     ifelse(cut_type == "with_cut", "With Cuts", "No Cuts"))
  } else {
    # Per-classifier plot: filter by classifier name prefix
    classifier_pattern <- paste0("^", classifier_name, ":")
    data <- histogram_combined %>%
      filter(grepl(classifier_pattern, label), cut_type == !!cut_type)
    title <- sprintf("%s - %s (%s)", sosname, classifier_name,
                     ifelse(cut_type == "with_cut", "With Cuts", "No Cuts"))
  }

  # Early return if no data matches the filter criteria
  if (nrow(data) == 0) {
    warning(sprintf("No data available for: %s", title))
    return(NULL)
  }

  # Calculate inverse cumulative sum statistics
  # Inverse cumsum = count of sources with probability >= current bucket
  if (is_merged) {
    # For merged: aggregate frequencies across all types within each bucket
    data <- data %>%
      group_by(bucket, bucket_start, bucket_end, bucket_mid) %>%
      summarise(freq = sum(freq), training_freq = sum(training_freq, na.rm = TRUE), .groups = "drop") %>%
      arrange(bucket) %>%
      mutate(
        # Forward cumsum: running total from low to high probability
        forward_cumsum = cumsum(freq),
        total_freq = sum(freq),
        # Inverse cumsum: sources with prob >= this bucket
        # Formula: total - (cumsum up to previous bucket) = total - forward_cumsum + current
        inverse_cumsum = total_freq - forward_cumsum + freq,
        inverse_cumsum_norm = inverse_cumsum / total_freq,
        # Same calculations for training data
        training_forward_cumsum = cumsum(training_freq),
        training_total_freq = sum(training_freq),
        training_inverse_cumsum = training_total_freq - training_forward_cumsum + training_freq,
        # Handle division by zero when no training data exists
        training_inverse_cumsum_norm = ifelse(training_total_freq > 0, training_inverse_cumsum / training_total_freq, 0)
      )
  } else {
    # For per-classifier: calculate cumsum separately for each type
    data <- data %>%
      group_by(label) %>%
      arrange(bucket) %>%
      mutate(
        forward_cumsum = cumsum(freq),
        total_freq = sum(freq),
        inverse_cumsum = total_freq - forward_cumsum + freq,
        inverse_cumsum_norm = inverse_cumsum / total_freq,
        training_forward_cumsum = cumsum(training_freq),
        training_total_freq = sum(training_freq),
        training_inverse_cumsum = training_total_freq - training_forward_cumsum + training_freq,
        training_inverse_cumsum_norm = ifelse(training_total_freq > 0, training_inverse_cumsum / training_total_freq, 0)
      ) %>%
      ungroup()
  }

  # Determine if log scale is appropriate based on frequency range
  # Use log scale when data spans more than 5x range
  positive_freqs <- data$freq[data$freq > 0]
  use_log <- FALSE
  if (length(positive_freqs) >= 2) {
    freq_range <- range(positive_freqs)
    use_log <- (freq_range[2] / freq_range[1]) > 5
  }

  # Create hover text for interactive tooltips
  if (is_merged) {
    data <- data %>%
      mutate(
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
    data <- data %>%
      mutate(
        # Extract type name by removing classifier prefix (e.g., "DSCTF:RRab" -> "RRab")
        type_name = sub("^[^:]+:", "", label),
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

  # Initialize empty plotly figure
  fig <- plot_ly()

  if (is_merged) {
    # === MERGED PLOT: Single aggregated bar series ===

    # Main frequency bars (steelblue)
    fig <- fig %>%
      add_trace(
        data = data %>% filter(freq > 0),
        x = ~bucket_mid,
        y = ~freq,
        type = 'bar',
        name = "All Types",
        text = ~hover_text,
        hoverinfo = 'text',
        marker = list(color = 'steelblue')
      )

    # Training data bars (green, semi-transparent)
    if (show_training) {
      fig <- fig %>%
        add_trace(
          data = data %>% filter(training_freq > 0),
          x = ~bucket_mid,
          y = ~training_freq,
          type = 'bar',
          name = "Training Sources",
          text = ~hover_text_training,
          hoverinfo = 'text',
          marker = list(color = 'green', opacity = 0.7)
        )
    }

    # Inverse cumulative sum line on secondary y-axis
    if (show_cumsum) {
      fig <- fig %>%
        add_trace(
          data = data,
          x = ~bucket_mid,
          y = ~inverse_cumsum,
          type = 'scatter',
          mode = 'lines',
          name = "Inv. Cumsum",
          line = list(width = 2, color = 'red'),
          yaxis = 'y2'
        )

      # Training inverse cumsum (dotted green line)
      if (show_training) {
        fig <- fig %>%
          add_trace(
            data = data %>% filter(training_freq > 0),
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
    # === PER-CLASSIFIER PLOT: Separate series for each type ===

    unique_labels <- unique(data$label)
    # Color palette for cycling through types
    colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
                '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')

    # Add traces for each type within this classifier
    for (i in seq_along(unique_labels)) {
      lbl <- unique_labels[i]
      data_subset <- data %>% filter(label == lbl)

      # Main frequency bars for this type
      fig <- fig %>%
        add_trace(
          data = data_subset,
          x = ~bucket_mid,
          y = ~freq,
          type = 'bar',
          name = sub("^[^:]+:", "", lbl),  # Display name without classifier prefix
          text = ~hover_text,
          hoverinfo = 'text',
          legendgroup = lbl,  # Group related traces in legend
          marker = list(color = colors[((i-1) %% length(colors)) + 1])
        )

      # Training bars for this type (green, semi-transparent)
      if (show_training) {
        fig <- fig %>%
          add_trace(
            data = data_subset %>% filter(training_freq > 0),
            x = ~bucket_mid,
            y = ~training_freq,
            type = 'bar',
            name = paste0(sub("^[^:]+:", "", lbl), " (Training)"),
            text = ~hover_text_training,
            hoverinfo = 'text',
            legendgroup = lbl,
            marker = list(color = 'green', opacity = 0.5)
          )
      }

      # Inverse cumsum line for this type
      if (show_cumsum) {
        fig <- fig %>%
          add_trace(
            data = data_subset,
            x = ~bucket_mid,
            y = ~inverse_cumsum,
            type = 'scatter',
            mode = 'lines+markers',
            name = paste0(sub("^[^:]+:", "", lbl), " (Cumsum)"),
            line = list(width = 2, color = colors[((i-1) %% length(colors)) + 1]),
            legendgroup = lbl,
            yaxis = 'y2'
          )

        # Training cumsum line for this type (dotted)
        if (show_training) {
          fig <- fig %>%
            add_trace(
              data = data_subset %>% filter(training_freq > 0),
              x = ~bucket_mid,
              y = ~training_inverse_cumsum,
              type = 'scatter',
              mode = 'lines',
              name = paste0(sub("^[^:]+:", "", lbl), " (Training Cumsum)"),
              line = list(width = 2, color = 'darkgreen', dash = 'dot'),
              legendgroup = lbl,
              yaxis = 'y2'
            )
        }
      }
    }
  }

  # Add vertical cut threshold lines
  if (show_cuts && !is.null(cuts_info[[sosname]])) {
    # Determine y-axis range for cut lines (extend slightly above max)
    y_max <- max(data$inverse_cumsum, na.rm = TRUE) * 1.1

    if (is_merged) {
      # For merged plot: show cut lines from ALL classifiers
      for (clf_name in names(cuts_info[[sosname]])) {
        clf_cuts <- cuts_info[[sosname]][[clf_name]]
        for (type_label in names(clf_cuts)) {
          cut_val <- clf_cuts[[type_label]]
          type_name <- sub("^[^:]+:", "", type_label)

          fig <- fig %>%
            add_trace(
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
    } else if (!is.null(cuts_info[[sosname]][[classifier_name]])) {
      # For per-classifier plot: show only cuts for this classifier
      clf_cuts <- cuts_info[[sosname]][[classifier_name]]
      for (type_label in names(clf_cuts)) {
        cut_val <- clf_cuts[[type_label]]
        type_name <- sub("^[^:]+:", "", type_label)

        fig <- fig %>%
          add_trace(
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

  # Configure plot layout with dual y-axes
  fig <- fig %>%
    layout(
      title = title,
      xaxis = list(title = "Posterior Probability"),
      yaxis = list(
        title = if (use_log) "Frequency (log)" else "Frequency",
        type = if (use_log) "log" else "linear"
      ),
      # Secondary y-axis for cumulative sum (overlaid on right side)
      yaxis2 = if (show_cumsum) list(
        title = "Inverse Cumsum",
        overlaying = 'y',
        side = 'right',
        type = if (use_log) "log" else "linear"
      ) else NULL,
      barmode = 'group',  # Group bars side-by-side
      hovermode = 'closest',
      legend = list(
        orientation = 'v',
        x = 1.15,
        y = 1
      )
    )

  # Save to HTML file if output directory specified
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # Construct filename based on plot type
    filename <- if (is_merged) {
      sprintf("%s_merged_%s.html", sosname, cut_type)
    } else {
      sprintf("%s_%s_%s.html", sosname, classifier_name, cut_type)
    }

    htmlwidgets::saveWidget(fig, file.path(output_dir, filename), selfcontained = TRUE)
  }

  return(fig)
}

#' @title Attribute Histogram Utilities
#' @description Functions for generating interactive plotly histograms from
#'   database column statistics. These utilities visualize value distributions
#'   across table columns with frequency bars and cumulative count overlays.
#' @name attribute-histograms
#' @keywords internal
NULL

library(plotly)
library(dplyr)


#' Determine if Logarithmic Scale Should Be Used for Frequency Axis
#'
#' Evaluates the range of frequency values to determine whether a logarithmic
#' scale would better visualize the distribution. Log scale is recommended
#' when the data spans multiple orders of magnitude.
#'
#' @param data A data frame containing a \code{freq} column with frequency
#'   counts. Typically a subset of histogram bucket data for a single
#'   table-column combination.
#'
#' @return Logical value: \code{TRUE} if log scale is recommended,
#'   \code{FALSE} otherwise.
#'
#' @details
#' The function applies the following logic:
#' \enumerate{
#'   \item Returns \code{FALSE} for NULL or empty data frames
#'   \item Filters to only positive frequencies (log scale requires > 0)
#'   \item Returns \code{FALSE} if fewer than 2 positive values exist
#'   \item Calculates the ratio of max to min frequency
#'   \item Returns \code{TRUE} if ratio exceeds 5x
#' }
#'
#' The 5x threshold is a heuristic that balances readability: distributions
#' with larger dynamic range benefit from log scale compression, while
#' narrow ranges are clearer on linear scale.
#'
#' @examples
#' \dontrun{
#' # Check if log scale needed for a data subset
#' df_subset <- sosHistAll %>%
#'   filter(table_name == "gaia_source", column_name == "parallax")
#'
#' use_log <- should_use_log_scale(df_subset)
#' # TRUE if max(freq)/min(freq) > 5
#' }
#'
#' @seealso \code{\link{create_attribute_histogram}} which calls this function
#'
#' @export
should_use_log_scale <- function(data) {
  # Handle NULL or empty input gracefully
  if (is.null(data) || nrow(data) == 0) return(FALSE)

  # Filter to positive frequencies only
  # Log scale is undefined for zero/negative values
  positive_freqs <- data$freq[data$freq > 0]

  # Need at least 2 values to compute a meaningful range
  if (length(positive_freqs) < 2) return(FALSE)

  # Calculate frequency range ratio
  freq_range <- range(positive_freqs, na.rm = TRUE)
  ratio <- freq_range[2] / freq_range[1]

  # Use log scale if range spans more than 5x
  # This threshold balances readability vs. compression
  return(ratio > 5)
}


#' Create Interactive Histogram for a Database Column Attribute
#'
#' Generates an interactive plotly histogram visualization for a specific
#' table-column combination from pre-computed bucket statistics. The plot
#' includes frequency bars with a cumulative count line overlay on a
#' secondary y-axis, with automatic scale selection.
#'
#' @param data A data frame containing histogram bucket data with columns:
#'   \describe{
#'     \item{table_name}{Character identifying the source database table}
#'     \item{column_name}{Character identifying the column within the table}
#'     \item{bucket}{Integer or numeric bucket index for ordering}
#'     \item{bucket_avg}{Numeric midpoint/average value for the bucket (x-axis)}
#'     \item{freq}{Integer frequency count for sources in this bucket}
#'     \item{nan_count}{Integer count of NULL/NaN values in the column}
#'     \item{non_nan_count}{Integer count of non-NULL values in the column}
#'   }
#' @param table_col_combo A list or single-row data frame with elements:
#'   \describe{
#'     \item{table}{Character string specifying the table name to filter}
#'     \item{column}{Character string specifying the column name to filter}
#'   }
#'
#' @return A plotly object containing the interactive histogram with:
#'   \itemize{
#'     \item Bar chart of frequency distribution (left y-axis)
#'     \item Cumulative count line (right y-axis)
#'     \item Title showing column name with table and count metadata
#'     \item Hover tooltips with bucket value and counts
#'     \item Automatic linear/log scale selection
#'   }
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Filters input data to the specified table-column combination
#'   \item Sorts by bucket and calculates cumulative frequency
#'   \item Extracts NaN/non-NaN counts for the subtitle
#'   \item Determines appropriate y-axis scale via \code{\link{should_use_log_scale}}
#'   \item Constructs dual-axis plotly visualization with scale indicator
#' }
#'
#' \strong{Automatic Log Scale:}
#' When the frequency range spans more than 5x, logarithmic scaling is
#' applied to both y-axes. The axis titles include "(Log Scale)" or
#' "(Linear Scale)" to indicate the current mode.
#'
#' \strong{Plot Title Format:}
#' The title displays the column name in bold, with a subtitle showing:
#' \code{table_name | Non-NaN: X | NaN: Y}
#'
#' \strong{Color Scheme:}
#' Both the frequency bars and cumulative line use the same blue color
#' (rgba(55, 128, 191)) for visual cohesion, with bars at 70 percent opacity.
#'
#' \strong{Hover Information:}
#' Tooltips display the column name, bucket value (2 decimal places),
#' and the corresponding frequency or cumulative count.
#'
#' @examples
#' \dontrun{
#' # Prepare table-column combination
#' combo <- list(table = "gaia_source", column = "phot_g_mean_mag")
#'
#' # Create histogram for this attribute
#' fig <- create_attribute_histogram(sosHistAll, combo)
#' fig
#'
#' # Or using a data frame row
#' combos <- data.frame(table = "gaia_source", column = "parallax")
#' fig <- create_attribute_histogram(sosHistAll, combos[1, ])
#' }
#'
#' @seealso \code{\link{should_use_log_scale}} for scale determination,
#'   \code{\link{brew_plotly_histogram_chunks}} for batch Rmd generation
#'
#' @importFrom plotly plot_ly add_bars add_lines layout
#' @importFrom dplyr filter arrange mutate
#'
#' @export
create_attribute_histogram <- function(data, table_col_combo) {

  # Filter data for specific table and column combination
  df_subset <- data %>%
    filter(table_name == table_col_combo$table,
           column_name == table_col_combo$column)

  # Calculate cumulative frequency for the overlay line
  # Sort by bucket to ensure correct cumulative ordering
  df_subset <- df_subset %>%
    arrange(bucket) %>%
    mutate(cumulative_freq = cumsum(as.numeric(freq)))

  # Extract metadata for subtitle display
  # These should be constant across all buckets for a given column
  nan_count <- unique(df_subset$nan_count)[1]
  non_nan_count <- unique(df_subset$non_nan_count)[1]

  # Determine if log scale should be used based on frequency range
  # Helps visualize distributions with large dynamic range
  use_log_scale <- should_use_log_scale(df_subset)

  # Create scale indicator text for axis labels
  # Informs user which scale mode is active
  scale_text <- if(use_log_scale) " (Log Scale)" else " (Linear Scale)"

  # Initialize empty plotly figure
  fig <- plot_ly()

  # Add histogram bars for frequency distribution
  # Semi-transparent fill with solid border for clarity
  fig <- fig %>%
    add_bars(
      data = df_subset,
      x = ~bucket_avg,
      y = ~freq,
      name = 'Frequency',
      marker = list(color = 'rgba(55, 128, 191, 0.7)',
                    line = list(color = 'rgba(55, 128, 191, 1.0)',
                                width = 1)),
      # Custom hover template shows column name and values
      hovertemplate = paste(
        '<b>', table_col_combo$column, ':</b> %{x:.2f}<br>',
        '<b>Frequency:</b> %{y}<br>',
        '<extra></extra>'
      )
    )

  # Add cumulative frequency line on secondary y-axis
  # Uses same color as bars for visual cohesion
  fig <- fig %>%
    add_lines(
      data = df_subset,
      x = ~bucket_avg,
      y = ~cumulative_freq,
      name = 'Cumulative',
      yaxis = 'y2',
      line = list(color = 'rgba(55, 128, 191, 1.0)', width = 2),
      hovertemplate = paste(
        '<b>', table_col_combo$column, ':</b> %{x:.2f}<br>',
        '<b>Cumulative:</b> %{y}<br>',
        '<extra></extra>'
      )
    )

  # Configure layout with dual y-axes and informative title
  fig <- fig %>%
    layout(
      # Title with column name (bold) and metadata subtitle
      title = list(
        text = paste0(
          '<b>', table_col_combo$column, '</b><br>',
          '<sup>', table_col_combo$table,
          ' | Non-NaN: ', format(non_nan_count, big.mark = ','),
          ' | NaN: ', format(nan_count, big.mark = ','), '</sup>'
        ),
        font = list(size = 16)
      ),
      # X-axis labeled with column name for context
      xaxis = list(title = table_col_combo$column),
      # Primary y-axis (left): frequency bars with scale indicator
      yaxis = list(
        title = paste0('Frequency', scale_text),
        side = 'left',
        type = if(use_log_scale) 'log' else 'linear'
      ),
      # Secondary y-axis (right): cumulative count line
      # Uses same scale type as primary for visual consistency
      yaxis2 = list(
        title = paste0('Cumulative Count', scale_text),
        overlaying = 'y',
        side = 'right',
        type = if(use_log_scale) 'log' else 'linear'
      ),
      hovermode = 'x unified',
      showlegend = TRUE,
      legend = list(x = 0.8, y = 1),
      # Extra right margin prevents y2 axis title cutoff
      margin = list(r = 100)
    )

  return(fig)
}

#' Plot aitoff
#'
#' @param skyMapD Skymap dataframe
#' @param valueName name of the value to plot from the dataframe
#' @param titleName Plot title
#' @param legendName Legend name
#' @param subtitleName Subtitle
#' @param hpxLevel 8 by default
#' @param point_alpha point to highlight if needed
#' @param point_delta point to highlight if needed
#' @param use_log_scale For big values
#' @param log_base log base
#'
#' @return ggplot
#' @export
#'

plotAitoffGalactic <- function(skyMapD, valueName, titleName, legendName,
                               subtitleName = NULL,
                               hpxLevel = 8,
                               point_alpha = NULL, point_delta = NULL,
                               use_log_scale = FALSE, log_base = 10) {
  library(viridis)
  library(rlang)

  skyMapFixed = skyMapD %>% mutate(alpha = ifelse(alpha>=0, alpha, 360 + alpha))

  skyMapGalactic = data.frame(skyMapFixed, togalactic(skyMapFixed$alpha, skyMapFixed$delta)) %>%
    mutate(aitoff_coords = aitoffFn(gl, gb),
           aitoffGl = aitoff_coords$x,
           aitoffGb = aitoff_coords$y)

  # Handle both string and symbol inputs for valueName
  if(is.character(valueName)) {
    valName <- sym(valueName)
    column_name <- valueName
  } else {
    valName <- ensym(valueName)
    column_name <- as_name(valName)
  }

  hpxDeg = dfHPX[dfHPX$Level==hpxLevel,"sq_deg"]
  theme <- theme_void() + theme(
    plot.title = element_text(hjust = 0.5, size=9),
    plot.subtitle = element_text(hjust = 0.5, size=7),
    plot.tag = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c(0, 0.5),
    legend.box.just = "left",
    legend.title.align = 0,
    legend.text.align = 0,
    aspect.ratio = 1/2
  )

  # Create the base plot
  p <- ggplot(skyMapGalactic, aes(x=aitoffGl, y=aitoffGb)) +
    ggtitle(titleName, subtitle = subtitleName) +
    geom_point(aes(colour = !!valName), alpha = .6, size = .15) +
    scale_x_reverse() +
    coord_fixed(ratio = 1) +
    theme

  # Configure color scale based on log option
  if (use_log_scale) {
    data_values <- skyMapGalactic[[column_name]]
    data_values <- data_values[!is.na(data_values) & data_values > 0]

    if(length(data_values) == 0) {
      stop("No positive values found for logarithmic scaling")
    }

    min_val <- min(data_values, na.rm = TRUE)
    max_val <- max(data_values, na.rm = TRUE)

    if(log_base == 10) {
      log_min <- floor(log10(min_val))
      log_max <- ceiling(log10(max_val))
      legend_breaks <- 10^seq(log_min, log_max, by = 1)

      if((log_max - log_min) <= 2) {
        intermediate_breaks <- c(2, 3, 4, 5, 6, 7, 8, 9)
        all_breaks <- c()
        for(i in log_min:log_max) {
          all_breaks <- c(all_breaks, 10^i)
          if(i < log_max) {
            intermediate <- intermediate_breaks * 10^i
            intermediate <- intermediate[intermediate < 10^(i+1) & intermediate >= min_val & intermediate <= max_val]
            all_breaks <- c(all_breaks, intermediate)
          }
        }
        legend_breaks <- sort(unique(all_breaks))
      }

      legend_breaks <- sort(unique(c(min_val, legend_breaks, max_val)))

      label_formatter <- function(x) {
        sapply(x, function(val) {
          if(is.na(val)) return(NA)
          log_val <- log10(val)
          if(abs(log_val - round(log_val)) < 1e-10) {
            return(as.character(val))
          } else {
            return(sprintf("%.2g", val))
          }
        })
      }

      trans_name <- "log10"
    } else {
      log_min <- log(min_val, base = log_base)
      log_max <- log(max_val, base = log_base)
      n_breaks <- 6
      legend_breaks <- log_base^seq(log_min, log_max, length.out = n_breaks)
      legend_breaks <- sort(unique(c(min_val, legend_breaks, max_val)))

      label_formatter <- function(x) {
        sapply(x, function(val) {
          if(is.na(val)) return(NA)
          return(sprintf("%.2g", val))
        })
      }

      if(log_base == exp(1)) {
        trans_name <- "log"
      } else {
        trans_name <- scales::log_trans(base = log_base)
      }
    }

    p <- p + scale_colour_viridis_c(
      option = "magma",
      breaks = legend_breaks,
      labels = label_formatter,
      trans = trans_name,
      na.value = "grey50",
      guide = guide_colourbar(
        title = bquote(atop(.(legendName), "per " ~ (.(hpxDeg) ~ Deg^2))),
        barwidth = 1.5,
        barheight = 20,
        title.position = "top",
        title.hjust = 0.5,
        ticks.linewidth = 1,
        label.position = "right",
        label.hjust = 0
      )
    )

  } else {
    # Linear scale
    data_values <- skyMapGalactic[[column_name]]
    data_values <- data_values[!is.na(data_values)]

    if(length(data_values) > 0) {
      min_val <- min(data_values, na.rm = TRUE)
      max_val <- max(data_values, na.rm = TRUE)

      if(min_val >= -0.1 && max_val <= 0.1) {
        legend_breaks <- c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1)
        legend_breaks <- legend_breaks[legend_breaks >= min_val & legend_breaks <= max_val]
        legend_breaks <- sort(unique(c(min_val, legend_breaks, max_val)))
      } else {
        n_breaks <- 8
        legend_breaks <- seq(min_val, max_val, length.out = n_breaks)
      }
    } else {
      legend_breaks <- c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1)
    }

    label_formatter <- function(x) {
      sapply(x, function(val) {
        if(is.na(val)) return(NA)
        if(abs(val) < 0.001) {
          return(sprintf("%.4f", val))
        } else if(abs(val) < 1) {
          return(sprintf("%.3f", val))
        } else {
          return(sprintf("%.2f", val))
        }
      })
    }

    p <- p + scale_colour_viridis_c(
      option = "magma",
      breaks = legend_breaks,
      labels = label_formatter,
      trans = "identity",
      guide = guide_colourbar(
        title = bquote(atop(.(legendName), "per " ~ (.(hpxDeg) ~ Deg^2))),
        barwidth = 1.5,
        barheight = 20,
        title.position = "top",
        title.hjust = 0.5,
        ticks.linewidth = 1,
        label.position = "right",
        label.hjust = 0
      )
    )
  }

  # Add red point if coordinates provided
  if (!is.null(point_alpha) && !is.null(point_delta)) {
    point_alpha_fixed <- ifelse(point_alpha >= 0, point_alpha, 360 + point_alpha)
    point_aitoff <- aitoffFn(point_alpha_fixed, point_delta)

    p <- p + geom_point(data = data.frame(x = point_aitoff$x, y = point_aitoff$y),
                        aes(x = x, y = y),
                        colour = "red",
                        size = 3,
                        alpha = 1,
                        inherit.aes = FALSE)
  }

  return(p)
}
