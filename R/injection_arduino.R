#' calculate injection rate from arduino files
#'
#' @param datelim
#' @param data
#' @param t_init
#' @param t_min
#' @param t_max
#' @param plot
#'
#' @return
#' @import readxl
#' @import lubridate
#' @import imuteTS
#' @export
#'
#' @examples
#' test
injection_arduino <- function(datelim,
                              data = NULL,
                              t_init = 1,
                              t_min = 3,
                              t_max = 5,
                              plot = "facets",
                              return_ls = T) {

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
      data$CO2[data$CO2 < 300| data$CO2 > 7000] <- NA
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
    data_sub <- merge(data_sub,probe1[,c("date","T_C")],all = T)
    data_sub$T_C[data_sub$T_C < -10| data_sub$T_C > 50] <- NA
    
    if(length(which(!is.na(data_sub$T_C))) > 1){
      data_sub$T_C <- imputeTS::na_interpolation(data_sub$T_C)
    }
    
    data_sub <- subset(data_sub,!is.na(inj))
  }else{
    data_sub$T_C <- NA
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
      
      #zeitraum kürzen zwischen t_init und t_max 
      data_sub$zeit[data_sub$zeit > t_max | data_sub$zeit < 0] <- NA
      
      #Die länge der jeweiligen Messungen nach der kürzung 
      len_messid <- sapply(seq_along(openingID), function(x) diff(range(data_sub$zeit[which(data_sub$messid == x)],na.rm=T)))
      #wenn die Effektiv benutzte Messzeit kleiner als t_min ist wird die Messung verworfen
      data_sub$zeit[data_sub$messid %in%  which(len_messid < t_min)] <- NA
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
        # p <- ggplot(flux_data)
        # p <- p+
        #   geom_line(aes(date,CO2,col=as.factor(messid),group=1))+
        #   labs(x="",y=expression(CO[2]))+
        #   guides(col=F)

        
        plt_data <- leave_NAtime_plot(data=flux_data,group="CO2",col="messid",plot=F)

        p <- ggplot(plt_data)+
          geom_line(aes(date,CO2,col=as.factor(messid),group=1))+
          geom_point(data = subset(plt_data, date %in% opening_date),aes(date,CO2,shape="opening"),col=1)+
          geom_point(data = subset(plt_data, date %in% closing_date),aes(date,CO2,shape="closing"),col=2)+
          guides(shape = guide_legend(override.aes = list(col = 2:1)))+
          facet_wrap(~period,scales="free_x",nrow=1)+
          theme(strip.text.x = element_blank())
        
        adj_grob_size(p,plt_data)
      }
      if(plot == "flux"){
        p <- ggplot(flux)+
          geom_line(aes(date,CO2_ml_per_min))+
          labs(x="",y=expression(inj~"("*ml ~ min^{-1}*")"),col="")
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


