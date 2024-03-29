% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{hc_theme}
\alias{hc_theme}
\title{Creating highcharter themes}
\usage{
hc_theme(...)
}
\arguments{
\item{...}{A list of named parameters.}
}
\description{
Highcharts is very flexible so you can modify every element of the chart.
There are some exiting themes so you can apply style to charts with few
lines of code.
}
\details{
More examples and details in \url{https://www.highcharts.com/docs/chart-design-and-style/themes}.
}
\examples{

hc <- highcharts_demo()

hc

thm <- hc_theme(
  colors = c("red", "green", "blue"),
  chart = list(
    backgroundColor = "#15C0DE"
  ),
  title = list(
    style = list(
      color = "#333333",
      fontFamily = "Erica One"
    )
  ),
  subtitle = list(
    style = list(
      color = "#666666",
      fontFamily = "Shadows Into Light"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = "Tangerine",
      color = "black"
    ),
    itemHoverStyle = list(
      color = "gray"
    )
  )
)

hc_add_theme(hc, thm)
}
