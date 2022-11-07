#' update the PP database
#'
#' @param table.name name of the table in the databese that shall be updated
#'
#' @return
#' @export
#' @import stringr
#' @import lubridate
#' @import dplyr
#' @import data.table
#' @import RSQLite
#' @import gsignal
#' @import RcppRoll
#'
#' @examples
update_PP.db<-function(table.name="PP_1min"){
  
  file_pattern <- "\\d+.+TXT"
  path <- PP_datapfad
  
  #namen der bereits in der db existierenden files laden
  if(file.exists(paste0(path,"db_log_",table.name,".txt"))){
    files.old<-read.csv(paste0(path,"db_log_",table.name,".txt"),stringsAsFactors = F)
  }else{
    files.old<-NULL
  }
  
  #alle csv-Dateien aus dem PP_data Ordner auflisten
  files<-list.files(path,file_pattern,full.names = F)
  
  
  #neue files auswählen
  files.new<-files[!files %in% files.old$x]
  
  ###################################################################
  #wenn neue files vorhanden sind...
  if(length(files.new)>0){
    print(paste("loading",length(files.new),"files"))
    #neue files laden
    
    data.list <- lapply(paste0(path,files.new),
                        read.table,
                        sep=";",
                        stringsAsFactors = F)
    
    
    
    
    data <- do.call(rbind,data.list)
    
    colnames(data) <- c(paste0("P_",1:6),"date_char")  
    #data$date <- strptime(data$date_char,"%Y-%m-%d %H:%M:%OS")
    data$date_int <- date_ms_as_int(data$date_char)
    
    date_duplicate <- duplicated(data$date_int)
    #are there date duplicates
    #if yes then the cases with duplicates are joined
    #replacing NA values of the duplicated rows with the values in the corresponding cases
    if(any(date_duplicate)){
      data <- rquery::natural_join(data[date_duplicate,],data[!date_duplicate,],by="date_int", jointype = "FULL")
    }
    
    data <- data[,c("date_int",paste0("P_",1:6))]
    
    
    if(is.null(data)){
      print(paste("no new files for",table.name))
    }else{
      
      #V to Pa
      data[,paste0("P_",1:6)] <- V_to_Pa(data[,paste0("P_",1:6)])*(-1)#* mal-1 weil die Sensoren falsch engschlossen sind und deshalb ein inverses Signal geben
      
      
      date <- date_ms_as_int(data$date_int)
      
      #points where time difference to previous point is more than one hour
      date_diff <- diff_time(date)
      from <- c(1,which(date_diff > 3600))
      to <- c(from[-1]-1,length(date))
      
      for(j in seq_along(from)){
        ID <- from[j]:to[j]
        t_diff <- median(diff_time(date[ID]),na.rm=T)
        
        #####################
        #P_filter und PPC
        fs <- 1 / round(t_diff,1)#1/s = Hz
        fpass <- c(0.003,0.1)
        wpass <- fpass / (fs /2)
        
        
        bpfilter <- gsignal::butter(n=3,w=wpass,type="pass")
        for(i in 1:6){
          data[ID,paste0("P_filter_",i)] <- gsignal::filtfilt(bpfilter,data[ID,paste0("P_",i)])
          data[ID,paste0("P_diff_",i)] <- abs(c(NA,diff(data[ID,paste0("P_filter_",i)])))
          data[ID,paste0("PPC_",i)] <- fs*RcppRoll::roll_mean(data[ID,paste0("P_diff_",i)],30*60*fs,fill=NA)
        }
      }
      colnames <- c("date_int",
                    paste0(rep(c("P_","P_filter_","P_diff_","PPC_"),each=6),rep(1:6,4))
      )
      
      if(table.name == "PP_1min"){
          
        data$date <- lubridate::round_date(date,"mins")
        data$P_horiz_1 <- abs(data$P_1 - data$P_2)
        data$P_horiz_2 <- abs(data$P_2 - data$P_3)
        data$P_horiz_3 <- abs(data$P_3 - data$P_4)
        
        colnames <- c("date_int",
                      paste0(rep(c("P_","P_filter_","P_diff_","PPC_"),each=6),rep(1:6,4)),
                      paste0(rep("P_horiz_",3),1:3)
                      )
        DT <- setDT(data)
        data_agg <- DT[,lapply(.SD,mean,na.rm=T),by=date,.SDcols = colnames]

        
        # data_agg <- data %>% 
        #   group_by(date) %>% 
        #   summarise(across(everything(),mean,na.rm=T)) %>% 
        #   select(-date) %>% 
        #   as.data.frame()
        data <- as.data.frame(data_agg)
        data$date_int <- date_ms_as_int(data$date)
        data <- data[order(data$date_int),colnames]
        data$date_int <- as.integer(data$date_int)
      }
      
      #db verbinden
      con <- RSQLite::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"PP.db"))
      #falls tabelle in db nicht vorhanden wird sie hier erstellt
      createquery<-paste0("CREATE TABLE IF NOT EXISTS ",table.name," (date_int INTEGER PRIMARY KEY",
                          paste(",",colnames[-1],"REAL",collapse = ""),")")
      DBI::dbExecute(con, createquery)
      
      print(paste("appending",length(files.new),"files to Database"))
      
      #Database aktualisieren
      DBI::dbWriteTable(con,name=table.name,value=data,append=T)
       # test <- DBI::dbReadTable(con,table.name)
       # DBI::dbRemoveTable(con,table.name)
      #disconnect Database
      DBI::dbDisconnect(con)
      
    }
    #files Datei speichern
    write.csv(c(files),
              paste0(path,"db_log_",table.name,".txt"),row.names = F)
  }else{#ende if files.new
    print(paste(table.name,"is up to date"))
  }
}#ende function

