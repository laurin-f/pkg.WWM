
#' Function to calculate ressure from the digital output of the Sensors 
#'
#' @param p_adc 
#'
#' @return
#' @export
#'
#' @examples
V_to_Pa <- function(p_adc){
  inH2O_to_mbar <-  2.49088874
  inH2O_to_pa <-  249.08890833333 
  
  Vsupply <-  5
  Pmin <-  -1
  Pmax <-  1
  
  voltage <-  (Vsupply/1023) * p_adc
  pressure_inH2O  <-  (-0.1 * Pmax * Vsupply + Pmax * voltage + 0.1 * Vsupply * Pmin - voltage * Pmin + ((0.8 * Pmax * Vsupply * Pmin - 0.8 * Vsupply * Pmin**2) / (Pmax - Pmin))) / (0.8 * Vsupply)
  p_Pa        <-  pressure_inH2O * inH2O_to_pa # Transform pressure data into Pa 
  return(p_Pa)
}