injection_arduino <- function(datelim,
                              data = NULL,
                              t_init = 0.5,
                              t_min = 3,
                              t_max = 5) {
  
  ########################
  #Metadaten aus Kammer laden
  
  Vol.xlsx<-readxl::read_xlsx(paste0(metapfad_tracer,"Diffusionskammer.xlsx"))
  Vol_ml<-Vol.xlsx$Volumen_effektiv_ml
  
  if(is.character(datelim)){
    datelim <- ymd_hm(datelim)
  }
  
  if(is.null(data)){
    files <- list.files(chamber_arduino_pfad,pattern = "_inj",full.names = F)
    
    
    ##subset of files with date in datelim
    file_date <- lubridate::ymd(stringr::str_extract(files,"^\\d{6}"))
    dates <- lubridate::date(datelim)
    files <- files[file_date >= dates[1] & file_date <= dates[2]]
    
    if(length(files)>0){
      #read files
      data_ls <- lapply(paste0(chamber_arduino_pfad,files),read.table,sep=";",header=T,stringsAsFactors = F,fill=T)
      names_char <- c("date","CO2_ppm","counter","inj")
      data_ls <- lapply(data_ls,"[",names_char)
      data <- do.call(rbind,data_ls)
      data$date <- ymd_hms(data$date)
      colnames(data) <- c("date","CO2","counter","inj")
      
      data <- data[order(data$date),]
      
      ###
      #aufbereiten
      data$CO2 <- as.numeric(data$CO2)
      data$CO2[data$CO2 < 300| data$CO2 > 7500] <- NA
      data$CO2 <- imputeTS::na_interpolation(data$CO2,maxgap = 10)
      
      data_sub <- subset(data,date > datelim[1] & date < datelim[2] & !is.na(data$date))
    }else{
      warning("no dynament data in datelim")
    }
  }else{
    data_sub <- subset(data,date > datelim[1] & date < datelim[2] & !is.na(data$date))
  }
  
  probe1 <- read_sampler(datelim=datelim,format="wide")
  if(nrow(probe1) > 0){
    probe1$T_C
    data_sub <- merge(data_sub,probe1[,c("date","T_C")],all = T)
    
    data_sub$T_C <- imputeTS::na_interpolation(data_sub$T_C)
    data_sub <- subset(data_sub,!is.na(inj))
  }
  ################
  #kammermessungen trennen
  if(exists("data_sub")){
    closingID <- which(diff(data_sub$inj) == 1)+1
    openingID <- which(diff(data_sub$inj) == -1)
    

    if(length(closingID) > 0 & length(openingID) > 0 ){
      if(closingID[1] > openingID[1]){
        closingID <- c(1,closingID)
      }
      
      if(tail(closingID,1) > tail(openingID,1)){
        openingID <- c(openingID,nrow(data_sub))
      }
      
      closing_date <- data_sub$date[closingID]
      opening_date <- data_sub$date[openingID]
      
      meas_time <- as.numeric(difftime(opening_date,closing_date,unit="mins"))
      
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
      
      
      flux_ls <- calc_flux(data = data_sub[!is.na(data_sub$CO2),],
                  group="messid",
                  Vol=Vol_ml,
                  gas = "CO2",
                  T_deg = "T_C")
      

      flux <- flux_ls[[1]]
      flux_data <- flux_ls[[2]]
      
      if(plot == "facets"){
        names(flux_data)
        p <- ggplot(subset(flux_data,!is.na(messid)))+
          geom_smooth(aes(zeit,CO2_tara),method="lm",se=F,col=1,linetype=2,lwd=0.7)+
          geom_line(aes(zeit,CO2_tara,col=as.factor(messid)))+
          #facet_wrap(~messid)+
          labs(y="CO2 (ppm) tara")+
          guides(col=F)
        
        if(max(flux_data$messid,na.rm=T) > 50){
          p <- p+
            facet_wrap(~ceiling(messid / 10))
        }else{
          p <- p+
            facet_wrap(~messid)
        }
        print(p)
      }
      if(plot == "timeline"){
        p <- ggplot(flux_data)
        p <- p+
          geom_line(aes(date,CO2,col=as.factor(messid),group=1))+
          labs(x="",y=expression(CO[2]))+
          guides(col=F)
        print(p)
      }
      if(plot == "flux"){
        p <- ggplot(flux)+
          geom_line(aes(date,get(paste0(gas[1],"_mumol_per_s_m2")),col=gas[1]))+
          labs(x="",y=expression(italic(F)~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")
        if(length(gas) > 1){
          p <- p+geom_line(aes(date,get(paste0(gas[2],"_mumol_per_s_m2")),col=gas[2]))
        }
        print(p)
      }
      
      
      if(return_ls){
        return(list(flux,flux_data))
      }else{
        return(flux)
      }
    }
  }#exists data_sub
}