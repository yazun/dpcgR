% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{foldTimeseriesFull}
\alias{foldTimeseriesFull}
\title{Folding timeseries function for sourceid and tag}
\usage{
foldTimeseriesFull(
  insourceid,
  intag,
  period,
  times,
  values,
  errors,
  referenceTime,
  range = 1.5
)
}
\arguments{
\item{insourceid}{sourceid}

\item{intag}{tag}

\item{period}{given period}

\item{times}{vector of obstimes}

\item{values}{vector of values}

\item{errors}{vector of errors}

\item{referenceTime}{reference time for folding}

\item{range}{range of the folded timeseries}
}
\value{
dataframe with folded indexes, phases, magnitudes and errors
}
\description{
Folding timeseries function for sourceid and tag
}
\examples{
\dontrun{
foldTimeseries(sourcied, tag, period, times, values, errors, referenceTime, range=1.5)
}
}
