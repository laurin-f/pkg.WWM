
# hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
# 
# datapfad_Pico<- paste0(hauptpfad,"Daten/Urdaten/Picolog/")
filenames <- c("230626.csv","230627.csv","230704.csv","230704_2.csv")

# filenames <- paste0(datapfad_Pico,"230705_P_test.csv")
# library(lubridate)
#' Title
#'
#' @param filenames 
#' @param full.names 
#' @param long 
#'
#' @return
#' @export
#'
#' @examples
read_Pico <- function(filenames,full.names = F,long = F) {
  ##########
  #read_data
  if(full.names == F){
    filenames <- paste0(datapfad_Pico,filenames)
  }
  data_ls <- lapply(filenames,read.table,sep = ",",header = T,dec = ",")
  data_ls <- mapply(cbind,data_ls,"file"=seq_along(data_ls),SIMPLIFY = F)
  
  ################
  #format
  for(i in seq_along(data_ls)){
    data_i <- data_ls[[i]]
    names(data_i) <- 
      stringr::str_replace_all(names(data_i),c("Mittel" = "_","\\." = "","X" = "date_int"))
    if("Grove_mV"%in%names(data_i)){
      data_i$Grove_V <- data_i$Grove_mV / 1000
      }
    data_i <- data_i[,c("date_int", "Grove_V", "SK25_mV", "KE50_mV", "RH_V", "T_V", "file")]
    data_ls[[i]] <- data_i
  }
  data <- do.call(rbind,data_ls)
  
  data$date <- as_datetime(data$date_int,tz = "CET")
  tz(data$date) <- "UTC"
  
  #################
  #Umrechnung von V
  sensors <- c("Grove","SK25","KE50")
  
  load(file = paste0(datapfad_Pico,"O2_fm.Rdata"))
  load(file = paste0(datapfad_Pico,"rh_T_fm.Rdata"))
  
  
  data$Grove_V <- RcppRoll::roll_mean(data$Grove_V,60,fill = NA)
  data$KE50_mV <- RcppRoll::roll_mean(data$KE50_mV,20,fill = NA)
  data$SK25_mV <- RcppRoll::roll_mean(data$SK25_mV,20,fill = NA)
  
  for(sensor in sensors){
    sensor_V <- grep(paste0(sensor,".+V"),names(O2_fm),value = T)
    data[,paste0(sensor,"_perc")] <- data[,sensor_V] * O2_fm[2,sensor_V] + O2_fm[1,sensor_V]
  }
  
  data$rh <- predict(rh_fm,newdata = data)
  data$T_C <- predict(T_fm,newdata = data)
  
  if(long){
    data <- tidyr::pivot_longer(data,matches(paste0(sensors,"_perc")),names_to = "sensor",values_to = "O2")
    data$sensor <- stringr::str_remove(data$sensor,"_perc")
    data <- data %>% 
      select(c(date,O2,sensor,rh,T_C))
  }
  
  return(data)
  
  
}