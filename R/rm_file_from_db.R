
#' Function to remove a specific file from the Database
#'
#' @param filename name of file
#' @param table_name name of table in db
#' @param db_name name of database
#' @param path path to file
#'
#' @return
#' @export
#'
#' @examples rm_file_from_db("^b0_work_20200526_102154_smp2.csv")
rm_file_from_db <- function(filename,
                            table_name = "sampler1",
                            db_name = "dynament.db",
                            file_path = dynpfad,
                            db_log_path = dynpfad){

  if(table_name %in% c("sampler1","sampler2","sampler1u2")){
  file <- read.csv(paste0(file_path,filename),stringsAsFactors = F)
  file <- file[-1,]
  date <- dmy_hms(paste(file[,1],file[,2]))
  
  db_log_file <- paste0(db_log_path,"db_log_",table_name,".txt")
  
  }
  if(table_name %in% c("micro","gga")){
    data_list <- lapply(paste0(file_path,filename),read.csv,skip=1)
    data <- do.call(rbind,data_list)
    date <- parse_date_time(data$Time,"mdYHMS")
    
    db_log_file <- paste0(db_log_path,table_name,"_files.txt")
  
  }
  datelim <- range(date,na.rm = T)

  query<-paste0("DELETE FROM ",table_name)
  #query2 <- paste0("SELECT datetime(date_int,'unixepoch') AS date,",cols," FROM ",table_name)
  #falls datelim angegeben wurde wird es hier als integer in die query aufgenommen

  from <- as.numeric(ymd_hms(datelim[1]))
  to <- as.numeric(ymd_hms(datelim[2]))
  #falls from oder to nicht ins ymd_hms format passt wird gestoppt

  #datelim in die SQL abfrage einbinden
  query<-paste0(query," WHERE date_int >= ",from," AND date_int <= ",to)
  # query2<-paste0(query2," WHERE date_int >= ",from," AND date_int <= ",to)

  con<-dbConnect(RSQLite::SQLite(),paste0(sqlpfad,db_name))
  #daten abrufen
  rs <- dbSendQuery(con,query)
  dbClearResult(rs)
  dbDisconnect(con)

  #auch aus db_log entfernen
  db_log<-read.csv(db_log_file,stringsAsFactors = F)
  db_log_new <- db_log[!db_log$x %in% filename,]
  #files Datei speichern
  write.csv(db_log_new,db_log_file,row.names = F)
}
