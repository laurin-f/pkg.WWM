

#' Function to
#'
#' @param data dataframe mit Messwerten
#' @param mod_dates Zeitpunkte zu denen modellierung statt finden soll
#' @param overwrite sollen existierende Modelloutputs überschrieben werden
#' @param read_all sollen alle Modellergebnisse der gewählten methode geladen werden
#' @param n_DS Anzahl an DS Schichten
#' @param plot sollen die Ergebnisse geplotten werden
#' @param offset_method name der methode zur offset korrektur \code{"gam"} oder \code{"drift" ...}
#' @param modelname name des COMSOL Projekts
#' @param z_soil_cm Tiefe des Modells in cm
#' @param file_suffix Dateiendung zB. um alternative Modellläufe zu kennzeichnen
#' @param comsolpath 
#'
#' @return
#' @export
#'
#' @examples
run_comsol_nruns <- function(data = data,
                             mod_dates,
                             overwrite = F,
                             read_all = T,
                             n_DS = 3,
                             plot = F,
                             offset_method = "drift",
                             z_soil_cm = 150,
                             modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs",
                             file_suffix = NULL,
                             comsolpath = comsolpfad,
                             long = T,
                             ...) {
  ############################################
  #Daten umformatieren
  ############################################
  
  data <- as.data.frame(data)
  n_DS_ch <- paste0(n_DS, "DS")
  
  #CO2 in tracer in mol pro m3
  if (!any(grepl("PressureActual_hPa", names(data)))) {
    print("no Pressure Data available using 101.3 kPa as default")
    data$PressureActual_hPa <- 1013
  }
  if (!any(grepl("T_soil", names(data)))) {
    print("no T_soil Data available using 20 °C as default")
    data$T_soil <- 20
  }
  
  #CO2 von ppm in mol/m3 umrechnen
  data$D0 <-
    D0_T_p(data$T_soil, p_kPa = data$PressureActual_hPa / 10, "m^2/s")
  data$CO2_mol_per_m3 <-
    ppm_to_mol(data[, paste0("CO2_tracer_", offset_method)],
               "ppm",
               p_kPa = data$PressureActual_hPa / 10,
               T_C = data$T_soil)
  #negative werte auf null setzen
  data$CO2_mol_per_m3[data$tiefe == 0] <- 0
  data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0] <- 0
  
  #model coordinates
  data$z <- z_soil_cm + data$tiefe
  data$r <- 0
  
  #Liste mit Werten an den Zeitpunkten die modelliert werden sollen
  data_sub <-
    lapply(mod_dates, function(x)
      data[data$date == x, c(
        "tiefe",
        "date",
        "CO2_mol_per_m3",
        "inj_mol_m2_s",
        "T_soil",
        "PressureActual_hPa",
        "CO2_ref"
      )])
  names(data_sub) <- mod_dates
  
  ##############################################
  #Comsol ausführen
  ##############################################
  #name für outputfiles
  date_chr <- format(mod_dates, "%y_%m_%d_%H_%M")
  outfile_names <-
    paste0(modelname, "_", offset_method, "_", date_chr, ".txt")
  
  if (!is.null(file_suffix)) {
    outfile_names <-
      paste0(modelname,
             "_",
             offset_method,
             "_",
             date_chr,
             "_",
             file_suffix,
             ".txt")
  }
  
  
  comsol_exe_nruns(modelname = modelname,
                   data_list = data_sub,
                   outfile_names = outfile_names,
                   outfile_raw = "CO2_optim.txt",
                   overwrite = overwrite,
                   ...)
  
  
  #####################################
  #Schleife um nacheinander die gewünschten Zeitpunkte dem Modell zu uebergeben
  
  
  #####################################################
  #COMSOL output
  ########################################################
  #alle dateien mit der gewünschten methode und datum einlesen (read_all)
  if (read_all == T) {
    date_pattern <- "\\d{2}(_\\d{2}){4}"
    file_pattern <-
      paste(modelname, offset_method, date_pattern, sep = "_")
    if (!is.null(file_suffix)) {
      file_pattern <-
        paste(modelname,
              offset_method,
              date_pattern,
              file_suffix,
              sep = "_")
    }
    outfile_names <-
      list.files(comsolpath, pattern = paste0(file_pattern, ".txt$"))
    #
    mod_date_all_chr <-
      sort(unique(str_extract(outfile_names, date_pattern)))
    
    
    mod_dates_all <- ymd_hm(mod_date_all_chr)
    mod_dates_all <- mod_dates_all[mod_dates_all %in% data$date]
    
    date_chr <- format(mod_dates_all, "%y_%m_%d_%H_%M")
    outfile_names <-
      paste0(modelname, "_", offset_method, "_", date_chr, ".txt")
    
    if (!is.null(file_suffix)) {
      outfile_names <-
        paste0(modelname,
               "_",
               offset_method,
               "_",
               date_chr,
               "_",
               file_suffix,
               ".txt")
    }
    #########################################
    #nur die Zeitpunkte mod_dates einlesen
  } else{
    mod_dates_all <- mod_dates
  }
  outfiles <- paste0(comsolpath, outfile_names)
  
  #Messwerte in Liste mit modellierten Zeitpunkten 
  data_list <-
    lapply(mod_dates_all, function(x)
      data[data$date == x,])
  names(data_list) <- as.character(mod_dates_all)
  
  F_Comsol <- data.frame(date = mod_dates_all)
  
  # print(length(outfiles))
  # print(all(file.exists(outfiles)))
  ####################################
  #read loop
  #######################################
  for (j in seq_along(mod_dates_all)) {
    lines <- readLines(outfiles[j])
    if(length(lines)>0){
      CO2_optim <- read.csv(outfiles[j],
                            skip = 9,
                            sep = "",
                            header = F)
      
      colnames(CO2_optim) <-
        c("r", "z", "CO2_mod_mol_m3", paste0("DS_", 1:n_DS))
      CO2_mod <- CO2_optim[, 1:3]
      best_DS <- CO2_optim[1, 4:(n_DS + 3)]
      
      #line <- readLines(outfiles[j],n = 1)
      #injection_rate <- stringr::str_split(line," ")[[1]][2]
      injection_rate <- CO2_optim[1,7]
      
      obs_j <- data_list[[as.character(mod_dates_all)[j]]]
      
      if (n_DS == 3) {
        schicht_grenzen <- c(0, -10.5, -21)
        tiefen <- c(-5.25, -15.75, -24.5)
      }
      
      schicht_untergrenzen <- c(schicht_grenzen[-1] + 0.01, -z_soil_cm)
      DS_profil <-
        data.frame(
          DS = unlist(best_DS),
          tiefe = tiefen,
          top = schicht_grenzen,
          bottom = schicht_untergrenzen
        )
      
      for (i in 1:nrow(DS_profil)) {
        tiefenID <-
          obs_j$tiefe <= DS_profil$top[i] &
          obs_j$tiefe > DS_profil$bottom[i]
        
        DS_profil$D0[i] <- mean(unlist(obs_j[tiefenID, c("D0")]))
      }
      
      
      ##################################################
      #Flux und DS in F_Comsol schreiben
      if (n_DS > 1) {
        
        for (k in 1:n_DS) {
          F_Comsol[F_Comsol$date == mod_dates_all[[j]], "mod_inj_rate"] <-
            injection_rate
          F_Comsol[F_Comsol$date == mod_dates_all[[j]], paste0("DSD0", k)] <-
            DS_profil$DS[k] / DS_profil$D0[k]
          F_Comsol[F_Comsol$date == mod_dates_all[[j]], paste0("DS", k)] <-
            DS_profil$DS[k]
        }
      } else{
        F_Comsol[F_Comsol$date == mod_dates_all[[j]], "DSD0"] <-
          best_DS / mean(obs_mod$D0, na.rm = T)
        F_Comsol[F_Comsol$date == mod_dates_all[[j]], "DS"] <- best_DS
      }
      
      
    }else{
      file.remove(outfiles[j])
    }
  }
  
  if(long == T){
    F_Comsol <- tidyr::pivot_longer(F_Comsol,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
    F_Comsol <- as.data.frame(F_Comsol)
  }
  return(F_Comsol)
}
