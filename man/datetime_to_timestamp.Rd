% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{datetime_to_timestamp}
\alias{datetime_to_timestamp}
\alias{dt_tstp}
\title{Date to timestamps}
\usage{
datetime_to_timestamp(dt)

dt_tstp(dt)
}
\arguments{
\item{dt}{Date or datetime vector}
}
\description{
Turn a date time vector to \code{timestamp} format
}
\examples{

datetime_to_timestamp(
  as.Date(c("2015-05-08", "2015-09-12"),
    format = "\%Y-\%m-\%d"
  )
)
}
