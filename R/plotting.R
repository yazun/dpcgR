

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
#' @importFrom ggplot2 theme_void element_text waiver
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

  valName = ensym(valueName)

  hpxDeg = dfHPX[dfHPX$Level==hpxLevel,"sq_deg"]

  theme <- theme_void() + theme(plot.title = element_text(hjust = 0.5, size=32),
                                plot.subtitle = element_text(hjust = 0.5),
                                plot.tag = element_text(hjust = 0.5)
  )

  ggplot(skyMapD,aes(x=aitoffGl,y=aitoffGb)  ) +
    ggtitle(titleName) +
    # geom_point(aes(colour = !!valName),alpha = .1, show.legend = FALSE) +
    geom_scattermore(aes(colour = !!valName),alpha = .4, show.legend = FALSE, pixels = c(1920,1080), pointsize = 3.2, interpolate = FALSE) +
    scale_x_reverse() +
    theme +
    # scale_colour_viridis_c(alpha= 0.2, option = "magma", trans="log10" , breaks = waiver()) +
    scale_colour_gradient(low = 'white', high = 'black', trans='log10', breaks = waiver())

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
#'
#' @return ggplot skymap plot overlayd over bacground plot
#' @export
#' @importFrom dplyr mutate
#' @importFrom ggplot2 guides
#'
#' @examples \dontrun{
#' # brew chunks on the xm.groups for a massive markdown generation
#' brewed.chunks = brew_chunks(xm.groups_big,plotAitoffGalacticOverlayBigSingleType,xm.groups_small,xm.skymap)
#' }
plotAitoffGalacticOverlayBigSingleType <-function (bkg, className, xm.skymap, hpxLevel = 8 )
{


  skyMapFixed.xm = xm.skymap %>%
    # filter(primaryvartype %in% classSet) %>%
    mutate(alpha = ifelse(alpha>= 0, alpha, 360 + alpha))

  if(count(skyMapFixed.xm)==0) return(bkg + ggtitle(paste(className,collapse=" "), subtitle = "No objects."));

  skyMapGalactic.xm = data.frame(skyMapFixed.xm, togalactic(skyMapFixed.xm$alpha, skyMapFixed.xm$delta)) %>%
    mutate(aitoffGl = aitoffFn(gl,gb)$x, aitoffGb = aitoffFn(gl,gb)$y)

  labelClass = paste(className,collapse=" ")
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
    ggtitle(labelClass, subtitle = paste("[",length(skyMapGalactic.xm),"] objects."))  +

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
                                    ,shape = factor(className)
                                    # ,fill = factor(skyMapGalactic.xm$primaryvartype)
      )
      , size = 1.6, name = "Types", alpha = .4
      , pixels = c(1920,1080), pointsize = 1.6, interpolate = FALSE) +
    # scale_colour_viridis_c(name = "Types", alpha= 0.6, option = "inferno", breaks = waiver(), labels = classSet, begin = beginColor, direction = -1) +
    scale_shape_manual(name = "Type", labels =  classSet, values = 1:length(className)) +
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
