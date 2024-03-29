% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plotCmdAndHRSeparateSets}
\alias{plotCmdAndHRSeparateSets}
\title{Plots CMD and HR diagrams in a combo using two, precomputed HR and CMD sets.
I.e. From sql queries.}
\usage{
plotCmdAndHRSeparateSets(
  inDataCmd,
  inData,
  valueName,
  catalogName = NULL,
  palette = "plasma",
  data.bkg.hr = data.bkg.hr,
  data.bkg.cmd = data.bkg.cmd
)
}
\arguments{
\item{inDataCmd}{dataframe with CMD data}

\item{inData}{dataframe with HR data}

\item{valueName}{name of the plots}

\item{catalogName}{unused for now. We could show literature ones here.}

\item{palette}{viridis palette name for density-colour of the points}

\item{data.bkg.hr}{data for Hr diagram}

\item{data.bkg.cmd}{data for cmd diagram}
}
\value{
two-plot ggplot with CMD and HR
}
\description{
Plots CMD and HR diagrams in a combo using two, precomputed HR and CMD sets.
I.e. From sql queries.
}
\examples{
\dontrun{
plotCmdAndHRSeparateSets(sosSetCmd,sosSetHr, valueName = cu7Name, catalogName = NULL)
}

}
