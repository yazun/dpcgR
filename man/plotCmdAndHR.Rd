% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plotCmdAndHR}
\alias{plotCmdAndHR}
\title{Plots CMD and HR diagrams in a combo}
\usage{
plotCmdAndHR(
  inData,
  valueName,
  catalogName = NULL,
  palette = "plasma",
  varpi_over_varpierror_cut = 5,
  adjuster = 5,
  data.bkg.hr = data.bkg.hr,
  data.bkg.cmd = data.bkg.cmd
)
}
\arguments{
\item{inData}{dataframe with CMD/HR data}

\item{valueName}{name of the plots}

\item{catalogName}{unused for now. We could show literature ones here.}

\item{palette}{viridis palette name for density-colour of the points}

\item{varpi_over_varpierror_cut}{cut for HR diagram}

\item{adjuster}{factor to adjust smoothing for density kernel}

\item{data.bkg.hr}{data for Hr diagram}

\item{data.bkg.cmd}{data for cmd diagram}
}
\value{
two-plot ggplot with CMD and HR
}
\description{
Plots CMD and HR diagrams in a combo
}
\examples{
\dontrun{
plotCmdAndHR(sosSet, valueName = cu7Name, catalogName = NULL)
}

}
