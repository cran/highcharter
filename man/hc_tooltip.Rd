% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highcharts-api.R
\name{hc_tooltip}
\alias{hc_tooltip}
\title{Tooltip options for highcharter objects}
\usage{
hc_tooltip(hc, ..., sort = FALSE, table = FALSE)
}
\arguments{
\item{hc}{A \code{highchart} \code{htmlwidget} object.}

\item{...}{Arguments defined in \url{https://api.highcharts.com/highcharts/tooltip}.}

\item{sort}{Logical value to implement sort according \code{this.point}
\url{https://stackoverflow.com/a/16954666/829971}.}

\item{table}{Logical value to implement table in tooltip:
\url{https://stackoverflow.com/a/22327749/829971}.}
}
\description{
Options for the tooltip that appears when the user hovers over a
series or point.
}
\examples{

highchart() \%>\%
  hc_add_series(data = sample(1:12)) \%>\%
  hc_add_series(data = sample(1:12) + 10) \%>\%
  hc_tooltip(
    crosshairs = TRUE,
    borderWidth = 5,
    sort = TRUE,
    table = TRUE
  )
}
