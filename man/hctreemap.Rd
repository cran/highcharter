% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hchart-shorcuts.R
\name{hctreemap}
\alias{hctreemap}
\title{Shortcut for create treemaps}
\usage{
hctreemap(tm, ...)
}
\arguments{
\item{tm}{A \code{treemap} object from the treemap package.}

\item{...}{Additional shared arguments for the data series
(\url{https://api.highcharts.com/highcharts/series}).}
}
\description{
This function helps to create highcharts treemaps from \code{treemap} objects
from the package \code{treemap}. NOTE: This function is deprecated. Please use \code{hctreemap2} instead.
}
\examples{
\dontrun{

library("treemap")
library("viridis")

data(GNI2014)
head(GNI2014)

tm <- treemap(GNI2014,
  index = c("continent", "iso3"),
  vSize = "population", vColor = "GNI",
  type = "comp", palette = rev(viridis(6)),
  draw = FALSE
)

hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") \%>\%
  hc_title(text = "Gross National Income World Data") \%>\%
  hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                             Pop: {point.value:,.0f}<br>
                             GNI: {point.valuecolor:,.0f}")
}

}
