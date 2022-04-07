#' update the PP database
#'
#' @param table.name name of the table in the databese that shall be updated
#'
#' @return
#' @export
#' @import stringr
#' @import lubridate
#' @import RSQLite
#' @import gsignal
#' @import RcppRoll
#'
#' @examples
update_PP.db<-function(table.name="PP_chamber"){
  
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
      data[,paste0("P_",1:6)] <- V_to_Pa(data[,paste0("P_",1:6)])
      
      date <- date_ms_as_int(data$date_int)
      t_diff <- as.numeric(median(difftime(date[-1],date[-nrow(data)],"secs")))
      
      #####################
      #P_filter und PPC
      fs <- 1 / round(t_diff,1)#1/s = Hz
      fpass <- c(0.003,0.1)
      wpass <- fpass / (fs /2)
      
      
      bpfilter <- gsignal::butter(n=3,w=wpass,type="pass")
      for(i in 1:6){
        data[,paste0("P_filter_",i)] <- gsignal::filtfilt(bpfilter,data[,paste0("P_",i)])
        abs_diff_i <- abs(c(NA,diff(data[,paste0("P_filter_",i)])))
        data[,paste0("PPC_",i)] <- fs*RcppRoll::roll_mean(abs_diff_i,30*60*fs,fill=NA)
      }
      
      #db verbinden
      con<-RSQLite::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"PP.db"))
      #falls tabelle in db nicht vorhanden wird sie hier erstellt
      createquery<-paste0("CREATE TABLE IF NOT EXISTS ",table.name," (date_int INTEGER PRIMARY KEY",
                          paste(",",paste0(rep(c("P_","P_filter_","PPC_"),each=6),rep(1:6,3)),"REAL",collapse = ""),")")
      DBI::dbExecute(con, createquery)
      
      print(paste("appending",length(files.new),"files to Database"))
      
      
      #Database aktualisieren
      DBI::dbWriteTable(con,name=table.name,value=data,append=T)
      
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

