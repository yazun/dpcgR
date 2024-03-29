

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
