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
#' @import ggplot2
#' @import imputeTS
#' @examples
chamber_arduino <- function(datelim,
                            data = NULL,
                            gga_data = T,
                            gas =  c("CO2","CO2_GGA","CH4"),
                            t_min = 5,
                            t_init = 1,
                            t_max = 10,
                            t_offset = 0,
                            gga = "gga",
                            plot = "facets",
                            return_ls = T) {
  Kammer <-
    readxl::read_xlsx(paste0(metapfad, "Kammermessungen/Kammer_Volumen.xlsx"),
                      sheet = "automatische Kammer")
  Vol <- Kammer$Kammer_Volumen_cm3
  Grundfl <- Kammer$Kammer_Grundfl_cm2
  
  ##########################################
  #daten laden
  if(is.null(data)){
    files <- list.files(chamber_arduino_pfad,pattern = "_chamber",full.names = F)
    
    if(is.character(datelim)){
      datelim <- ymd_hm(datelim)
    }
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
  }else{
    data_sub <- subset(data,date > datelim[1] & date < datelim[2] & !is.na(data$date))
  }
  
  ###########
  #data_sub
  
  
  #########
  #read GGA
  if(gga_data == T){
    data_gga <- read_GGA(datelim =datelim,table.name = gga)
    if(nrow(data_gga) > 0){
      data_gga <- data_gga[,1:4]
      data_gga$date <- round_date(data_gga$date,"5 secs") - t_offset
      names(data_gga) <- c("date","CO2_GGA","CH4","H2O")
      #names(data_gga) <- c("date",paste0(names(data_gga[-1]),"_GGA"))
      
      if(exists("data_sub")){
        data_agg <- data_sub %>% 
          mutate(date = ceiling_date(date, "5 secs")) %>% 
          group_by(date) %>% 
          summarise(CO2 = mean(CO2,na.rm=T),
                    T_C = mean(T_C,na.rm=T),
                    chamber=max(chamber))
        
        data_sub <- merge(data_agg,data_gga,all=T)
        data_sub$chamber <- imputeTS::na_interpolation(data_sub$chamber,method="constant")
      }else{
        data_sub <- data_gga
      }
    }else{
      print(paste0("no ", gga, " data in datelim"))
    }
  }
  if(!all(gas %in% colnames(data_sub))){
    warning("colums ",paste(gas[!gas %in% names(data_sub)],collapse = " and ")," don't exist and are removed from gas")
    gas <- gas[gas %in% names(data_sub)]
  }
  
  if(exists("data_sub")){
    ################
    #kammermessungen trennen
    
    closingID <- which(diff(data_sub$chamber) == 1)+1
    openingID <- which(diff(data_sub$chamber) == -1)
    
    if(length(closingID) > 0 & length(openingID) > 0 ){
      if(closingID[1] > openingID[1]){
        closingID <- c(1,closingID)
      }
      
      if(tail(closingID,1) > tail(openingID,1)){
        openingID <- c(openingID,nrow(data_sub))
      }
      
      closing_date <- data_sub$date[closingID]
      opening_date <- data_sub$date[openingID]
      
      meas_time <- as.numeric(difftime(opening_date,closing_date,"mins"))
      
      closingID <- closingID[meas_time > t_min]
      openingID <- openingID[meas_time > t_min]
      
      data_sub$zeit <- NA
      data_sub$messid <- NA
      for (i in 1:length(openingID)) {
        #zeit in minuten nach closing
        data_sub$zeit[closingID[i]:openingID[i]] <-
          difftime(data_sub$date[closingID[i]:openingID[i]], data_sub$date[closingID[i]]+t_init*60, unit =
                     "mins")
        #messid als durchlaufende Nummer f�r jede closing opening periode
        data_sub$messid[closingID[i]:openingID[i]] <- i
      }
      data_sub$zeit[data_sub$zeit > t_max | data_sub$zeit < 0] <- NA
      data_sub$messid[is.na(data_sub$zeit)] <- NA
      
      
      flux <- lapply(gas,function(x) 
        calc_flux(data = data_sub[!is.na(data_sub[,x]),],
                  group="messid",
                  Vol=Vol,
                  Grundfl = Grundfl,
                  gas = x,
                  T_deg = "T_C"))
      
      flux_ls <- lapply(flux,"[[",1)
      flux_ls <- lapply(flux_ls,function(x) {
        x$date <- lubridate::round_date(x$date,"10 mins")
        x})
      
      data_ls <- lapply(flux,"[[",2)
      
      data_merge <- Reduce(function(...) merge(..., all=T),data_ls)
      
      flux_merge <- Reduce(function(...) merge(..., by=c("date","messid"), all=T),flux_ls)
      
      
      if(plot == "facets"){
        
        p <- ggplot(subset(data_merge,!is.na(messid)))+
          geom_smooth(aes(zeit,get(paste0(gas[1],"_tara"))),method="lm",se=F,col=1,linetype=2,lwd=0.7)+
          geom_line(aes(zeit,get(paste0(gas[1],"_tara")),col=gas[1]))+
          #facet_wrap(~messid)+
          labs(y=paste0(gas[1],"(ppm) tara"))+
          guides(col=F)
        if(length(gas) > 1){
          p <- p+geom_line(aes(zeit,get(paste0(gas[2],"_tara")),col=gas[2]))
        }
        if(max(data_merge$messid,na.rm=T) > 50){
          p <- p+
            facet_wrap(~ceiling(messid / 10))
        }else{
          p <- p+
            facet_wrap(~messid)
        }
        print(p)
      }
      if(plot == "timeline"){
        p <- ggplot(data_merge)
        if(length(gas) > 1){
          p <- p+
            geom_line(aes(date,get(gas[2]),col=gas[2],group=1))+
            ggnewscale::new_scale_color()
        }
        p <- p+
          geom_line(aes(date,get(gas[1]),col=as.factor(messid),group=1))+
          labs(x="",y=gas[1])+
          guides(col=F)
        print(p)
      }
      if(plot == "flux"){
        p <- ggplot(flux_merge)+
          geom_line(aes(date,get(paste0(gas[1],"_mumol_per_s_m2")),col=gas[1]))+
          labs(x="",y=expression(italic(F)~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")
        if(length(gas) > 1){
          p <- p+geom_line(aes(date,get(paste0(gas[2],"_mumol_per_s_m2")),col=gas[2]))
        }
        print(p)
      }
      
      
      if(return_ls){
        return(list(flux_merge,data_merge))
      }else{
        return(flux_merge)
      }
    }
  }#exists data_sub
}

