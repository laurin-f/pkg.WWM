% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comsol_exe.R
\name{comsol_exe}
\alias{comsol_exe}
\title{Function to execute Comsol Modell with defined parameters}
\usage{
comsol_exe(
  modelname,
  input_pars = NULL,
  outfile_new = NULL,
  outfile_raw = "CO2_flux_prod.txt",
  COMSOL_progammpath = COMSOL_progammpfad,
  COMSOL_exepath = COMSOL_exepfad,
  job = "b1",
  overwrite = F
)
}
\arguments{
\item{modelname}{name of the Comsol Modell without .mph ending}

\item{input_pars}{named vector or dataframe where names represent Parameter names as defined in Comsol}

\item{outfile_new}{name that the COMSOL outputfile is changed to from this function}

\item{outfile_raw}{name that COMSOL gives the outputfile}

\item{COMSOL_progammpath}{path to COMSOL project file}

\item{COMSOL_exepath}{path to comsolbatch.exe}

\item{job}{name of the job that is defined in the COMSOL project file}

\item{overwrite}{should existing modellresults be overwritten or not}
}
\value{

}
\description{
Function to execute Comsol Modell with defined parameters
}
\examples{
comsol_exe(model="Produktionseimer",input_pars=input_pars_9,outfile="CO2_flux_prod_9.txt")
}
