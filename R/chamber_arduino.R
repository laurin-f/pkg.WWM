#' FUnction to calculate CO2 flux from arduino measurements
#'
#' @param datelim ymd_hm
#' @param data default = NULL data is loaded from inj_arduino_pfad
#' @param return_ls F = return flux data.frame if T = returns list with [[1]] = flux and [[2]] data
#'
#' @return
#' @export
#' @import lubridate
#' @import readxl
#' @import imputeTS
#' @examples
chamber_arduino <- function(datelim,
                            data = NULL,
                            return_ls = F) {
  Kammer <-
    readxl::read_xlsx(paste0(metapfad, "Kammermessungen/Kammer_Volumen.xlsx"),
                      sheet = "automatische Kammer")
  Vol <- Kammer$Kammer_Volumen_cm3
  Grundfl <- Kammer$Kammer_Grundfl_cm2
  
  ##########################################
  #daten laden
  if(is.null(data)){
  files <- list.files(chamber_arduino_pfad,pattern = "_chamber",full.names = T)
  
  data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F,fill=T)
  data <- do.call(rbind,data_ls)
  data$date <- ymd_hms(data$date)
  colnames(data) <- c("date","CO2","T_C","chamber")
  
  data <- data[order(data$date),]
  
  ###
  #aufbereiten
  data$CO2 <- as.numeric(data$CO2)
  data$CO2[ data$CO2 < 300| data$CO2 > 9000] <- NA
  data <- data[-which(diff(data$CO2) < -200),]
  data$CO2 <- imputeTS::na_interpolation(data$CO2,maxgap = 10)
  
  data$T_C <- as.numeric(data$T_C)
  data$T_C[data$T_C < -10| data$T_C > 60 |data$T_C == 0] <- NA
  data$T_C[which(abs(diff(data$T_C)) > 1)] <- NA
  }
  
  ###########
  #data_sub
  if(is.character(datelim)){
    datelim <- ymd_hm(datelim)
  }
  
  data_sub <- subset(data,date > datelim[1] & date < datelim[2])

  ################
  #kammermessungen trennen
  
  closingID <- which(diff(data_sub$chamber) == 1)+1
  openingID <- which(diff(data_sub$chamber) == -1)+1
  
  if(closingID[1] > openingID[1]){
    closingID <- c(1,closingID)
  }
  
  if(tail(closingID,1) > tail(openingID,1)){
    openingID <- c(openingID,nrow(data_sub))
  }
  
  data_sub$zeit <- NA
  data_sub$messid <- NA
  for (i in 1:length(openingID)) {
    #zeit in minuten nach closing
    data_sub$zeit[closingID[i]:openingID[i]] <-
      difftime(data_sub$date[closingID[i]:openingID[i]], data_sub$date[closingID[i]], unit =
                 "mins")
    #messid als durchlaufende Nummer f�r jede closing opening periode
    data_sub$messid[closingID[i]:openingID[i]] <- i
  }
  
  flux <- calc_flux(na.omit(data_sub),group="messid",Vol=Vol,Grundfl = Grundfl,T_deg = "T_C")
  if(return_ls){
    return(flux)
  }else{
    return(flux[[1]])
  }
  
}



