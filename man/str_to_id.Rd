% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{str_to_id}
\alias{str_to_id}
\alias{str_to_id_vec}
\title{String to 'id' format}
\usage{
str_to_id(x)

str_to_id_vec(x)
}
\arguments{
\item{x}{A vector string.}
}
\description{
Turn a string to \code{id} format used in treemaps.
}
\examples{

str_to_id(" A string _ with sd / sdg    Underscores \   ")
}
