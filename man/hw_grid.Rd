% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hw_grid.R
\name{hw_grid}
\alias{hw_grid}
\title{Lays out highchart widgets into a "grid", similar to
\code{grid.arrange} from \code{gridExtra}.}
\usage{
hw_grid(
  ...,
  ncol = NULL,
  rowheight = NULL,
  add_htmlgrid_css = TRUE,
  browsable = TRUE
)
}
\arguments{
\item{...}{either individual \code{highchart} objects or a mixture of
individual \code{highchart} objects and \code{list}s of \code{highchart} objects.}

\item{ncol}{how many columns in the grid}

\item{rowheight}{Height in px.}

\item{add_htmlgrid_css}{A logical value to add or not \code{htmlgrid.css} as
dependency.}

\item{browsable}{Logical value indicating if the returned object is converted
to an HTML object browsable using \code{htmltools::browsable}.}
}
\description{
Lays out highchart widgets into a "grid", similar to
\code{grid.arrange} from \code{gridExtra}.
}
\examples{

charts <- lapply(1:9, function(x) {
  hchart(ts(cumsum(rnorm(100))))
})

if (interactive()) {
  hw_grid(charts, rowheight = 300)
}
}
