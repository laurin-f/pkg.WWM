
#' Function to convert units from ppm to mol / m^3 or reverse
#'
#' @param data vector with values that should be converted from ppm to mol or reverse
#' @param unit_in unit of input data if this is ppm output will be in mol / m^3 and vice versa
#' @param out_class class of the output as character eg. \code{"numeric"} (default) or \code{"units"}
#' @param p_kPa pressure in kPa
#' @param T_C Temperature in °C
#' @importFrom units set_units
#' @return vector with converted units
#' @export
#'
#' @examples ppm_to_mol(CO2_ppm,"ppm")
#' ppm_to_mol(0.0168,"mol/m^3")
ppm_to_mol <- function(data,
                       unit_in="ppm",
                       out_class="numeric",
                       p_kPa = 101.3,
                       T_C = 20){

  data_in <- set_units(data,unit_in,mode="standard")
  p_kPa <- set_units(p_kPa,"kPa")
  T_C <- set_units(T_C,"°C")
  T_K <- set_units(T_C,"K")
  p <- set_units(p_kPa,"kg/(m*s^2)")
  R <- set_units(8.314, "kg*m^2/(s^2*mol*K)")
  mol_per_m3 <- p/(R*T_K)
  mol_per_m3

  if(unit_in == "ppm"){
    data_out <- data_in * mol_per_m3 / set_units(10^6,"ppm")
  }else if(unit_in == "mol/m^3"){
    data_out <- data_in / mol_per_m3 *set_units(10^6,"ppm")
  }else if(unit_in == "cm^3/min"){
    data_out <- data_in * mol_per_m3
  }
  return(as(data_out,out_class))
}

#' Function to calculate temperature dependent D0 in cm2/s
#'
#' @param gas CO2 or CH4
#' @param T_C Temperature in Celsius
#' @param p_kPa pressure in kPa
#'
#' @return
#' @export
#'
#' @examples D0_temp(15)
D0_T_p <- function(T_C = 20,
                   p_kPa = 101.325,
                   unit = "cm^2/s",
                   gas = "CO2"){

  D0 <- c(CO2 = 0.1381, CH4= 0.1952) # cm2/s

  T0 <- 273.15 #K
  p0 <- 101.325 #kPa


  alpha <- 1.81
  T_K <- T_C + 273.15

  D0_T_p_cm2_s <- D0[gas] * (T_K/T0)^alpha * (p0/p_kPa)
  D0_T_p <- change_unit(D0_T_p_cm2_s,"cm^2/s",unit)
  return(D0_T_p)}
