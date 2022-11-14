#################################################
#Funktion um daten aus db abzurufen
#' @title Function to get data from a sql database
#' @description get data from a sql database. A specific time period can be selected
#' @param db.name name of the database
#' for "dynament.db" either \code{"dynament_test"} or \code{"samplerx"}
#' for "GGA.db" either \code{"gga"} or \code{"micro"}
#' @param table.name name of the table in the database
#' @param datelim  limits of the time period that is loaded as character or POSIXct if all Data should be loaded \code{datelim = NULL}
#' @param cols names of the colums that will be loaded. default is all colums: \code{cols = "*"}
#' @param sqlpath path to the database

#'
#' @return data.frame
#' @export
#' @import lubridate
#' @import RSQLite
#' @examples
#' #Zeitrahmen festlegen
#' datelim<-c("2020-01-15 10:00:00","2020-01-21 10:00:00")
#'
#' #read_db anwenden
#' dynament_raw<-read_db("dynament.db","dynament_test",datelim,korrektur_dyn=F)
#' GGA<-read_db("GGA.db","micro",datelim,"CO2,CO2dry")
read_GGA <- function(db.name="GGA.db", #name der db
                    table.name="micro", #name der tabelle
                    datelim=NULL, #Zeitrahmen der geladen werden soll
                    cols="*", #Spalten die geladen werden sollen
                    sqlpath = sqlpfad,
                    ggapath = ggapfad){
  #dbs updaten
  update_fun<-get(paste0("update_",db.name))
  update_fun(table.name,path=ggapath,sqlpath)

  #db verbinden
  con<-odbc::dbConnect(RSQLite::SQLite(),paste0(sqlpath,db.name))

  #date_int wird als datum formatiert
  query<-paste0("SELECT datetime(date_int,'unixepoch') AS date,",cols," FROM ",table.name)

  #falls datelim angegeben wurde wird es hier als integer in die query aufgenommen
  if(!is.null(datelim)){
    from_to <- as.numeric(ymd_hms(datelim))
    #falls from oder to nicht ins ymd_hms format passt wird gestoppt
    if(any(is.na(from_to))){
      stop("datelim has to be in the ymd_hms format")
    }
    #datelim in die SQL abfrage einbinden
    query<-paste0(query," WHERE date_int >= ",from_to[1])
    if(length(from_to)==2){
      query<-paste0(query," AND date_int <= ",from_to[2])
    }
  }

  #daten abrufen
  data<-DBI::dbGetQuery(con,query)
  #von db trennen
  odbc::dbDisconnect(con)
  #datum formatieren
  data$date<-ymd_hms(data$date)
  #date_int weglassen
  data<-data[,colnames(data)!="date_int"]

  #Spalten die nur aus NAs bestehen weglassen
  na.cols <- apply(data,2,function(x) any(is.na(x)==F))
  data<-data[,na.cols]


  return(data)
}#ende function
