% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proxy.R
\name{hcpxy_update_series}
\alias{hcpxy_update_series}
\title{Update options series in a higchartProxy object}
\usage{
hcpxy_update_series(proxy, id = NULL, ...)
}
\arguments{
\item{proxy}{A \code{higchartProxy} object.}

\item{id}{A character vector indicating the \code{id} (or \code{id}s) of the series to update.}

\item{...}{Arguments defined in \url{https://api.highcharts.com/highcharts/plotOptions.series}.
The arguments will be the same for each series. So if you want update data it
is used this function sequentially for each series.}
}
\description{
Update options series in a higchartProxy object
}
