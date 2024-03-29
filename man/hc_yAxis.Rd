% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highcharts-api.R
\name{hc_yAxis}
\alias{hc_yAxis}
\title{Yaxis options for highcharter objects}
\usage{
hc_yAxis(hc, ...)
}
\arguments{
\item{hc}{A \code{highchart} \code{htmlwidget} object.}

\item{...}{Arguments defined in \url{https://api.highcharts.com/highcharts/yAxis}.}
}
\description{
The Y axis or value axis. Normally this is the vertical axis,
though if the chart is inverted this is the horizontal axis.
In case of multiple axes, the yAxis node is an array of
configuration objects.
See the Axis object for programmatic
access to the axis.
}
\examples{

highchart() \%>\%
  hc_add_series(
    data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
    type = "spline"
  ) \%>\%
  hc_yAxis(
    title = list(text = "y Axis at right"),
    opposite = TRUE,
    alternateGridColor = "#FAFAFA",
    minorTickInterval = "auto",
    minorGridLineDashStyle = "LongDashDotDot",
    showFirstLabel = FALSE,
    showLastLabel = FALSE,
    plotBands = list(
      list(
        from = 13,
        to = 17,
        color = "rgba(100, 0, 0, 0.1)",
        label = list(text = "This is a plotBand")
      )
    )
  )
}
