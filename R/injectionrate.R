
#' Function to calculate Injectionrate for specific Diffusionchamber measurement
#'
#' @param datelim time intervall
#' @param Pumpstufen verwendete Pumpstufen bei den unterschiedlichen Versuchen
#' @param group name of the column that should be used for grouping results
#' @param return_data if T then the function returns a list with the flux as first element and the processed input data as second element
#' @param ... #other parameters parsed to calc_flux
#' @param closing_lim
#' @param opening_lim
#' @param t_max
#' @param t_init
#' @param t_min
#' @param T_C either a numeric value for one Temperature or the name of the Temperture column as character
#' @param spikes_th
#' @param difftime_th
#' @param all_spikes_NA
#' @param data
#' @param colname name of the CO2 column in the df
#'
#' @return
#' @export
#'
#' @examples split <- injectionrate(datelim = datelim,Pumpstufen = c(1:5,5,rep(NA,4),1:4,1:5),group="Pumpstufe",
#' closing_before = 20,
#' closing_after = 20,
#' opening_before = 0,
#' opening_after = 10,
#' t_max=6,
#' t_init = 1,
#' t_min=3)
injectionrate <- function(datelim,
                          Pumpstufen,
                          group = "Pumpstufe",
                          closing_lim = 100,
                          opening_lim = 0,
                          t_max=6,
                          t_init = 1,
                          t_min=2,
                          T_C = NA,
                          spikes_th = 500,
                          difftime_th = 10,
                          all_spikes_NA = F,
                          data = NULL,
                          return_data=F,
                          colname = "CO2",
                          adj_openings=T,
                          ...){

  ########################
  #Daten einlesen
  ########################
  #Metadaten aus Kammer laden

  Vol.xlsx<-readxl::read_xlsx(paste0(metapfad_tracer,"Diffusionskammer.xlsx"))
  Vol_ml<-Vol.xlsx$Volumen_effektiv_ml

  #CO2 daten einlesen
  if(is.null(data)){
    data <- read_db("dynament.db","dynament_test",datelim = datelim)
  }
  #Spaltenname anpassen
  data$CO2_raw <- data[,colname]
  data$CO2 <- data$CO2_raw
  data$CO2[which(data$CO2 < 0)] <- NA
  data <- subset(data,!is.na(CO2))
  ##########################
  #postprocessing
  #############################
  #spikes entfernen
  spikes <- which(abs(diff(data$CO2_raw)) > spikes_th & diff(as.numeric(data$date)) <= difftime_th)+1
  data$CO2[spikes[spikes %in% (spikes - 1)]] <- NA
  if(all_spikes_NA){
    data$CO2[spikes] <- NA
  }
  #split_chmaber anwenden
  split <- split_chamber(data,
                         closing_lim = closing_lim,
                         opening_lim = opening_lim,
                         t_max=t_max,
                         t_init = t_init,
                         t_min=t_min,
                         adj_openings = adj_openings)

  #Pumpstufen den messid's zuordnen
  if(length(Pumpstufen) == 1){
    split$Pumpstufe <- NA
    split$Pumpstufe[!is.na(split$messid)] <- Pumpstufen
  }else{
    split$Pumpstufe <-
      as.character(factor(split$messid,
                                     levels = unique(split$messid),
                                     labels=Pumpstufen))
    split$Pumpstufe <- as(split$Pumpstufe,class(Pumpstufen))
  }


  #Fluss mit calc_flux bestimmen
  flux <- calc_flux(data = split,
                    Vol = Vol_ml,
                    group=group,
                    T_deg = T_C,
                    ...)

  #
  if(return_data == T){
    return(flux)
  }else{
    return(flux[[1]])
  }
}
