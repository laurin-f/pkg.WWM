#' Title
#'
#' @param datelim 
#'
#' @return
#' @export
#'
#' @examples
read_arduino_chamber <- function(datelim){
  files <- list.files(chamber_arduino_pfad,pattern = "_chamber",full.names = F)
  
  
  ##subset of files with date in datelim
  file_date <- lubridate::ymd(stringr::str_extract(files,"^\\d{6}"))
  dates <- lubridate::date(datelim)
  files <- files[file_date >= dates[1] & file_date <= dates[2]]
  
  if(length(files)>0){
    #read files
    data_ls <- lapply(paste0(chamber_arduino_pfad,files),read.table,sep=";",header=T,stringsAsFactors = F,fill=T)
    names_char <- c("date","CO2_ppm","temp_C","chamber")
    data_ls <- lapply(data_ls,"[",names_char)
    data <- do.call(rbind,data_ls)
    data$date <- ymd_hms(data$date)
    colnames(data) <- c("date","CO2","T_C","chamber")
    
    data <- data[order(data$date),]
    
    ###
    #aufbereiten
    data$CO2 <- as.numeric(data$CO2)
    data$CO2[ data$CO2 < 300| data$CO2 > 9000] <- NA
    data <- data[!(diff(data$CO2) < -200),]
    data$CO2 <- imputeTS::na_interpolation(data$CO2,maxgap = 10)
    
    data$T_C <- as.numeric(data$T_C)
    data$T_C[data$T_C < -10| data$T_C > 60 |data$T_C == 0] <- NA
    data$T_C[which(abs(diff(data$T_C)) > 1)] <- NA
    data_sub <- subset(data,date > datelim[1] & date < datelim[2] & !is.na(data$date))
  }else{
    warning("no dynament data in datelim")
  }
  return(data_sub)
}
