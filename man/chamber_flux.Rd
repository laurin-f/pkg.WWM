% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chamber_flux.R
\name{chamber_flux}
\alias{chamber_flux}
\title{function to calculate chamber flux}
\usage{
chamber_flux(
  messnr = NULL,
  mess_dir = "Hartheim",
  datelim = NULL,
  GGA = NULL,
  chamber = NULL,
  T_deg = NA,
  p_kPa = NA,
  return_data = F,
  aggregate = F,
  metapath = metapfad,
  ggapath = ggapfad,
  sqlpath = sqlpfad,
  ...
)
}
\arguments{
\item{messnr}{which measurement should be used from metadata. default is NULL = all measurements are used}

\item{mess_dir}{directory in which the metadata of the measurements is stored}
}
\value{

}
\description{
function to calculate chamber flux
}
