#' function to calculate chamber flux
#'
#' @param mess_dir directory in which the metadata of the measurements is stored
#' @param messnr which measurement should be used from metadata. default is NULL = all measurements are used
#'
#' @return
#' @export
#'
#' @examples
chamber_gga <- function(datelim = NULL,
                         GGA = "gga",
                         T_deg = NA,
                         p_kPa = NA,
                         return_data = F,
                         aggregate = F,
                         metapath = metapfad,
                         ggapath = ggapfad,
                         sqlpath = sqlpfad,
                         #wenn return_data == T wird als Output eine Liste mit den bearbeiteten INput Daten ausgegeben
                         ...) {
  ########################
  #Metadaten laden
  #######################
  
  #in sheet 2 habe ich abgelegt wie hoch die Kragen aus dem Boden ragen
  
  kragen <-
    readxl::read_xlsx(paste0(metapfad,"PP_Kammer/Kammermessungen.xlsx"), sheet = 2)
  
  
  
  #im Kammer_Meta.xslsx habe ich das Kammervolumen abgeschätzt
  Kammer <-
    readxl::read_xlsx(paste0(metapath, "Kammermessungen/Kammer_Volumen.xlsx"),
                      sheet = "automatische Kammer")
  Schlauch_meta <-
    readxl::read_xlsx(paste0(metapath, "Kammermessungen/Kammer_Volumen.xlsx"),
                      sheet = "schlauch_volumen")
  #Hier steht drin welches interne Volumen die GGAs haben
  GGA_Vol <-
    readxl::read_xlsx(paste0(metapath, "Kammermessungen/GGA_volumen.xlsx"))
  
  
  kragen$height <- kragen$height_cm
  
  ###########################
  #Datum formatieren
  #anfang und ende der Messungen als date
  if(is.character(datelim)){
      datelim <- lubridate::ymd_hm(datelim)
    }
    #beginn <- min(datelim)
    #ende <- max(datelim) 
    
    kragen_id <- tail(which(datelim[1]-kragen$datum > 0),1)
    height <- kragen$height_cm[kragen_id]
    
  ###########################
  #Daten laden
  #daten aus Database einlesen
  
  
  data.raw <- read_GGA("GGA.db", GGA, datelim = datelim,ggapath=ggapath,sqlpath=sqlpath)
  
  ########################
  #split chamber function um die Zeiträume der einzelmessungen zu bestimmen
  data <- split_chamber(data = data.raw,
                        ...)
  data$dummy <- 1
  ############################
  #Volumen bestimmen
  ############################
  Vol_GGA <- GGA_Vol$Volume_effektive_cm3[GGA_Vol$Analyzer == GGA]
  
  #schlauch_cols <- grep("schlauch", colnames(Messungen), value = T)
  # Vol_schlauch_df <-
  #   Messungen[, schlauch_cols] * Schlauch_meta[rep(3, nrow(Messungen)), schlauch_cols]
  #Vol_schlauch <- rowSums(Vol_schlauch_df) #cm3
  Vol_schlauch <- 100 * as.numeric(Schlauch_meta[3,"schlauch_blau_6mm"])
  
  
    #Vol in cm3
    Grundfl <- Kammer$Kragen_Grundfl_innen_cm2 #cm2
    #Grundfl_aussen <- Kammer$Kragen_Grundfl_aussen_cm2
    
    Hoehe <- Kammer$Kragen_Hoehe_cm#cm
    Vol_kammer <- Kammer$Kammer_Volumen_cm3
    
    Vol_basis <-
      Grundfl * height + Vol_kammer
    #Vol_schlauch<-20 #cm3
    Vol <-
      Vol_basis + Vol_schlauch +
      Vol_GGA#cm3
  
  
  
  ###########################
  #Fluss berechnen
  ###########################
  
  #Liste für Output anlegen
  flux <- list()
  
    #Wenn alle Schlauchlaengen gleich sind
    for (i in c("CO2", "CH4")) {
      # if(is.na(T_deg)){
      #T_deg <- mean(data$AmbT_C[data$date > beginn_seq &
      #                            data$date < ende_seq], na.rm = T)
      #
      # }
      flux[[i]] <- calc_flux(
        data = data,
        group = "dummy",
        gas = i,
        Vol = Vol + unique(Vol_schlauch),
        Grundfl = Grundfl,
        aggregate = aggregate,
        T_deg = T_deg,
        p_kPa = p_kPa
      )
    }
  data_adj <- merge(flux[["CO2"]][[2]],flux[["CH4"]][[2]])
  flux_both <- merge(flux[["CO2"]][[1]],flux[["CH4"]][[1]])
  if(return_data == T){
    return(list(flux=flux_both,data=data_adj))
  }else{
    return(flux_both)
  }
}
