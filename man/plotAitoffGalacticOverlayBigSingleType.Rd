% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plotAitoffGalacticOverlayBigSingleType}
\alias{plotAitoffGalacticOverlayBigSingleType}
\title{Plot overlay distribution in a skyplot with a single class attribute}
\usage{
plotAitoffGalacticOverlayBigSingleType(
  bkg,
  className,
  xm.skymap,
  alpha = "ra_deg",
  delta = "dec_deg",
  hpxLevel = 8,
  palette = "plasma",
  adjuster = 6
)
}
\arguments{
\item{bkg}{background plot we overlay on}

\item{className}{name of the primaryvartype to filter on (for classification.)}

\item{xm.skymap}{dataframe to plot, with alpha, delta fields}

\item{alpha}{name of alpha in Deg in df}

\item{delta}{name of delta in Deg in df}

\item{hpxLevel}{expected hpxLevel of the input.}

\item{palette}{name of the viridis palette to use for the type.}

\item{adjuster}{weight for density smoothing by geom_pointdensity}
}
\value{
ggplot skymap plot overlayd over background plot
}
\description{
Uses geom_scattermore for plotting.
}
\examples{
\dontrun{
# brew chunks on the xm.groups for a massive markdown generation
brewed.chunks =
  brew_chunks(xm.groups_big
  ,plotAitoffGalacticOverlayBigSingleType
   , bkg
   , sosType
   , sosConfigName)
}
}
