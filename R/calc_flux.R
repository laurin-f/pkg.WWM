##########################
#
#' Function to calculate flux from chamber measurements
#'
#' @param data dataset with gas concentration measurements over time
#' @param group name of the column that is used for grouping measurements ACHTUNG group sollte eine "_" enthalten da in der Funktion str_split(,"_")
#' @param gas name of the gas
#' @param Vol chamber Volum in ml (cm3)
#' @param Grundfl area in m2
#' @param P_kPA pressure in kPa
#' @param T_deg Temperature in °C
#' @param aggregate sollen die Werte nach group aggregiert werden
#'
#' @return data.frame with flux in different units for each group
#' @import dplyr
#' @export
#'
#' @examples calc_flux(split,Vol=Vol_ml,tracer_conc = 100)
calc_flux <- function(data,
                      group = "kammer",
                      gas = "CO2",
                      Vol,#in cm3
                      Grundfl=NULL,#in cm2
                      p_kPa = 101.3,
                      T_deg = NA,
                      aggregate = F){

  ##############
  #Tara
  ################
  #CO2_tara als CO2-anstieg von Nullpunkt
  #eine liste mit den gas.tara werten
  gas.tara_list <- lapply(na.omit(unique(data$messid)), function(x){
    #indizes der reihen mit messid == x
    messid.x <- data$messid == x
    #anfangszeitpunkt der messid
    min.zeit <- min(data$zeit[messid.x],na.rm = T)
    #jeden gas wert der messid minus den gas wert zu zeit == min.zeit
    data[,gas][which(messid.x)] - data[,gas][which(messid.x & data$zeit == min.zeit)]
  })

  #Spalte gas_tara anhängen
  data[,paste0(gas, "_tara")] <- NA
  #und befülen mit den tara werten
  data[,paste0(gas, "_tara")][!is.na(data$messid)] <- do.call(c,gas.tara_list)

  ###################
  #group und Messid
  ###################

  #spalte mit group und messid zusammen
  group_messid <- paste0(group,"_messid")
  data[,group_messid] <- paste0(data[,group],"_",data$messid)

  #NAs aus dem character zu echten NAs umwandeln
  NA_NA <- grep("NA", data[,group_messid])
  data[NA_NA,group_messid] <- NA

  #unique Werte
  group_messid_unique <- na.omit(unique(data[,group_messid]))
  #die group und messid wieder außeinanderschneiden
  gr_id <- str_split(group_messid_unique,"_",simplify = T)
  
  #########################
  #Concentration slope
  ########################

  #Formel für glm
  formula <- paste0(gas,"_tara ~ zeit")
  #für jeden werte von group wird eine regression zwische gas und zeit durchgeführt
  fm_list <- lapply(1:nrow(gr_id), function(x) glm(formula,data = subset(data,messid == gr_id[x,2])))
  #aus der fm_liste wird jeweils der zweite coeffizient (steigung) ausgeschnitten
  ppm_per_min <- sapply(fm_list,"[[","coefficients")[2,]#ppm/min
  
  #der R2 der regression wird bestimmt
  #Funktion um R2 aus fm zu bestimmen
  R2_fm <- function(fm){
    R2 <- 1-fm$deviance/fm$null.deviance
    return(R2)
  }
  
  #Funktion auf Liste anwenden
  R2_str <- sapply(fm_list,R2_fm)
  #Werte unter 95% identifizieren
  bad_meas <- which(R2_str < 0.8)
  #dazugehörige messid
  bad_messid <- sapply(fm_list[bad_meas],function(x) unique(x$data$messid))
  #Warnung
  if(length(bad_meas)>0){
    daterange <- sapply(fm_list[bad_meas],function(x) paste(format(range(x$data$date),"%H:%M:%S"),collapse = " to "))
    print(paste(gas,"measurement",bad_messid,"from",daterange,"has an R2 of:",round(R2_str[bad_meas],2)))
  }

  if(is.character(T_deg)){
    T_df <- data %>% 
      dplyr::group_by(messid) %>% 
      dplyr::filter(!is.na(messid)) %>% 
      dplyr::summarise_at(T_deg,list(~mean(.,na.rm=T))) %>% 
      as.data.frame()
    
    T_deg <- T_df[,T_deg]
  }
  #######################################
  #Konstanten um Einheiten umzurechnen
  #######################################
  #Luftdruck
  p_Pa <- p_kPa*1000#PA = N/m2 = kg/(m s2)
  #p_Pa <- set_units(p_kPa*1000,"kg/m/s^2")#PA = N/m2 = kg/(m s2)
  #Temperatur
  T_K <- T_deg+273.15 #K
  #T_K <- set_units(T_deg+273.15,"K") #K
  #allgemeine Gaskonstante
  R <- 8.314 #kg m2/(s2 mol K)
  #R <- set_units(8.314, "kg*m^2/s^2/mol/K") #kg m2/(s2 mol K)
  #Molare Masse CO2 und CH4
  M<-data.frame(CO2 = 44.01, CH4 = 16.04)#g/mol
  #M<-data.frame(CO2 = set_units(44.01, "g/mol"), CH4 = set_units(16.04, "g/mol")) #g/mol

  #dichte
  ro_g_per_m3 <- p_Pa*M/(R*T_K) #kg/(m s2) * g/mol / kg m2 * (s2 mol K)/ K = g/m3
  m_mol_per_m3 <- p_Pa/(R*T_K) #kg/(m s2) / kg /m2 * (s2 mol K)/ K = mol/m3
  ro<- ro_g_per_m3 / 10^6 #g/cm3 = g/ml
  m<- m_mol_per_m3 / 10^6 #mol/cm3 = mol/ml

  #berechnung Flux in unterschiedlichen Einheiten
  flux <- data.frame(ppm_per_min,R2=R2_str)
  #flux <- data.frame(ppm_per_min)

  if(length(Vol) > 1){
    Vol <- as.numeric(as.character(factor(gr_id[,1],levels=names(Vol),labels=Vol)))
  }

  ########################################
  #flux in unterschiedlichen Einheiten
  ########################################

  flux$ml_per_min<-ppm_per_min /10^6 * Vol #cm3 / min
  flux$g_per_min <- flux$ml_per_min * ro[,substr(gas,0,3)] #g / min
  flux$mol_per_min <- flux$ml_per_min * m #mol / min
  flux$mumol_per_s <- change_unit(flux$mol_per_min,"mol/min","micromol/s") #mol / min
  if(!is.null(Grundfl)){
    flux$ml_per_min_m2<-flux$ml_per_min/(Grundfl/10^4) #cm3 /(min m2)
    flux$g_per_min_m2 <- flux$g_per_min/(Grundfl/10^4) #g/(min m2)
    flux$mol_per_min_m2 <- flux$mol_per_min/(Grundfl/10^4) #mol/(min m2)
    flux$mumol_per_s_m2 <- flux$mumol_per_s/(Grundfl/10^4) #mumol/(s m2)
  }

  #group spalte an flux anfügen
  flux$messid <- as.numeric(gr_id[,2])
  #date

  ###################
  #Mittelwerte von date und Temperatur
  #mittelwerte des Datums der unterschiedlichen gruppen
  date_means <- sapply(1:nrow(gr_id), function(x) mean(data[which(data$messid == gr_id[x,2]),"date"]))
  flux$date <- lubridate::as_datetime(date_means)

  #T_C ist bei mir die Temperatur in der Tracerinjektiobskiste
  # if("T_C" %in% colnames(data)){
  #   T_C_means <- sapply(1:nrow(gr_id), function(x) mean(data[which(data$messid == gr_id[x,2]),"T_C"]))
  #   flux$T_C <- T_C_means
  # }
  flux$T_C <- T_deg

  if(group == "messid"){
    #wenn messid nicht die Gruppe ist dan wird es jetzt entfernt
    data <- data[,!grepl(group_messid,colnames(data))]
  }
  ##################
  #aggregate
  ##################
  if(aggregate == T){
    #flux nach group aggregieren
    flux <- aggregate(flux,list("group" = gr_id[,1]),mean)
    flux$date <- with_tz(flux$date, "UTC")

    if(group != "messid"){
      #wenn messid nicht die Gruppe ist dan wird es jetzt entfernt
      flux <- flux[,!grepl("messid",colnames(flux))]
    }
    #der Spaltenname "group" wird in group umgewandelt
    colnames(flux) <- str_replace(colnames(flux),"group",group)
  }else{
    
    #wenn nicht agggegiert wird dann wird hier die Spalte mit group angehängt
    flux[,group] <- gr_id[,1]
  }
  #der Name des Gases wird am Anfang jeder Spalte mit einer Einheit vorangestellt
  colnames(flux) <- stringr::str_replace(colnames(flux),"^(?=(ppm|ml|g|mol|mumol|R2))",paste0(gas,"_"))
  #liste als output
  return(list(flux=flux,data=data))
}



