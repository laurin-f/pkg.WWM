% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_Em50.R
\name{read.Em50}
\alias{read.Em50}
\title{Function to read Data from Em50Data.db}
\usage{
read.Em50(format = "long", ...)
}
\arguments{
\item{format}{either \code{"long"} (default) or \code{"wide"}}

\item{...}{other parameters parsed to read_db such as datelim or cols
(see help of read_db)}
}
\value{

}
\description{
reads in Data from Em50Data.db and formats it if wanted to long format
}
\examples{
read.Em50("long",datelim = c("2020.01.01 00:00:00","2021.01.01 00:00:00"))
}
