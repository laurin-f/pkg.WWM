#' update the PP database
#'
#' @param table.name name of the table in the databese that shall be updated
#'
#' @return
#' @export
#' @import stringr
#' @import lubridate
#' @import RSQLite
#'
#' @examples
update_PP.db<-function(table.name="PP_chamber"){
  
  file_pattern <- "\\d{8}.+TXT"
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
      dyn <- rquery::natural_join(dyn[date_duplicate,],dyn[!date_duplicate,],by="date_int", jointype = "FULL")
    }
    
    data <- data[,c("date_int",paste0("P_",1:6))]
    
    if(is.null(data)){
      print(paste("no new files for",table.name))
    }else{
      #db verbinden
      con<-RSQLite::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"PP.db"))
      #falls tabelle in db nicht vorhanden wird sie hier erstellt
      createquery<-paste0("CREATE TABLE IF NOT EXISTS ",table.name," (date_int INTEGER PRIMARY KEY",
                          paste(",",paste0("P_",1:6),"INTEGER",collapse = ""),")")
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

