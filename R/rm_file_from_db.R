
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
                            path = dynpfad){

  file <- read.csv(paste0(path,filename),stringsAsFactors = F)
  file <- file[-1,]
  date <- dmy_hms(paste(file[,1],file[,2]))
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

  con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,db_name))
  #daten abrufen
  rs <- dbSendQuery(con,query)
  dbClearResult(rs)
  dbDisconnect(con)

  #auch aus db_log entfernen
  db_log<-read.csv(paste0(path,"db_log_",table_name,".txt"),stringsAsFactors = F)
  db_log_new <- db_log[!db_log$x %in% filename,]
  #files Datei speichern
  write.csv(db_log_new,
            paste0(path,"db_log_",table_name,".txt"),row.names = F)
}
