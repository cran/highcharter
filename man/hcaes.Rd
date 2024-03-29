% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highcharts-api-add.R
\name{hcaes}
\alias{hcaes}
\title{Define aesthetic mappings.
Similar in spirit to \code{ggplot2::aes}}
\usage{
hcaes(x, y, ...)
}
\arguments{
\item{x, y, ...}{List of name value pairs giving aesthetics to map to
variables. The names for x and y aesthetics are typically omitted because
they are so common; all other aesthetics must be named.}
}
\description{
Define aesthetic mappings.
Similar in spirit to \code{ggplot2::aes}
}
\examples{

hcaes(x = xval, color = colorvar, group = grvar)
}
