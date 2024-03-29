% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highcharts-api-add.R
\name{hc_add_series.lm}
\alias{hc_add_series.lm}
\alias{hc_add_series.loess}
\title{hc_add_series for lm and loess objects}
\usage{
\method{hc_add_series}{lm}(
  hc,
  data,
  type = "line",
  color = "#5F83EE",
  fillOpacity = 0.1,
  ...
)

\method{hc_add_series}{loess}(
  hc,
  data,
  type = "line",
  color = "#5F83EE",
  fillOpacity = 0.1,
  ...
)
}
\arguments{
\item{hc}{A \code{highchart} \code{htmlwidget} object.}

\item{data}{A \code{lm} or \code{loess} object.}

\item{type}{The type of the series: line, spline.}

\item{color}{A stringr color.}

\item{fillOpacity}{fillOpacity to the confidence interval.}

\item{...}{Arguments defined in
\url{https://api.highcharts.com/highcharts/chart}.}
}
\description{
hc_add_series for lm and loess objects
}
