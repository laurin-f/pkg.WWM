% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_flux.R
\name{calc_flux}
\alias{calc_flux}
\title{Function to calculate flux from chamber measurements}
\usage{
calc_flux(
  data,
  group = "kammer",
  gas = "CO2",
  Vol,
  Grundfl = NULL,
  p_kPa = 101.3,
  T_deg = NA,
  aggregate = F
)
}
\arguments{
\item{data}{dataset with gas concentration measurements over time}

\item{group}{name of the column that is used for grouping measurements ACHTUNG group sollte eine "_" enthalten da in der Funktion str_split(,"_")}

\item{gas}{name of the gas}

\item{Vol}{chamber Volum in ml (cm3)}

\item{Grundfl}{area in m2}

\item{T_deg}{Temperature in °C}

\item{aggregate}{sollen die Werte nach group aggregiert werden}

\item{P_kPA}{pressure in kPa}
}
\value{
data.frame with flux in different units for each group
}
\description{
Function to calculate flux from chamber measurements
}
\examples{
calc_flux(split,Vol=Vol_ml,tracer_conc = 100)
}
