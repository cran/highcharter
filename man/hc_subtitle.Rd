% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highcharts-api.R
\name{hc_subtitle}
\alias{hc_subtitle}
\title{Subtitle options for highcharter objects}
\usage{
hc_subtitle(hc, ...)
}
\arguments{
\item{hc}{A \code{highchart} \code{htmlwidget} object.}

\item{...}{Arguments defined in \url{https://api.highcharts.com/highcharts/subtitle}.}
}
\description{
The chart's subtitle. This can be used both to display a subtitle below
the main title, and to display random text anywhere in the chart. The
subtitle can be updated after chart initialization through the
Chart.setTitle method.
}
\examples{

highchart() \%>\%
  hc_add_series(
    data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
    type = "column"
  ) \%>\%
  hc_subtitle(
    text = "And this is a subtitle with more information",
    align = "left",
    style = list(color = "#2b908f", fontWeight = "bold")
  )
}
