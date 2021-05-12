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
#' @param korrektur_dyn logical specifies whether dynament values are corrected for the specific sansor id or not if db.name is "dynament.db"
#' @param metapath path to the "korrektur_fm.RData" file for correction factors
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
read_db <- function(db.name="dynament.db", #name der db
                    table.name="dynament_test", #name der tabelle
                    datelim=NULL, #Zeitrahmen der geladen werden soll
                    cols="*", #Spalten die geladen werden sollen
                    sqlpath=sqlpfad,
                    korrektur_dyn=T,#sollen die Dynament werden korrigiert werden
                    metapath=metapfad_dyn){
  #dbs updaten
  update_fun<-get(paste0("update_",db.name))
  update_fun(table.name)

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
  data<-dbGetQuery(con,query)
  #von db trennen
  odbc::dbDisconnect(con)
  #datum formatieren
  data$date<-ymd_hms(data$date)
  #date_int weglassen
  data<-data[,colnames(data)!="date_int"]

  #Spalten die nur aus NAs bestehen weglassen
  na.cols <- apply(data,2,function(x) any(is.na(x)==F))
  data<-data[,na.cols]

  #wenn bei dynament.db die tabelle dynament_test ausgewählt ist können die korrektur_faktoren angewandt werden
  if(db.name=="dynament.db" & table.name %in% c("dynament_test","sampler3_raw") & korrektur_dyn==T){
    #RData mit Korrekturfaktoren laden
    load(paste0(metapath,"korrektur_fm.RData"))
    if(table.name == "sampler3_raw"){
    names(fm) <- stringr::str_replace(names(fm),"_sampler3","")
    }
    #Vektor mit namen die sowohl bei den Korekturfaktoren als auch im Data.frame vorkommen
    same.names <- names(fm)[names(fm) %in% colnames(data)]
    #Sensor nummern für die noch kein Korrekturfaktor vorliegt
    missing.names <- colnames(data)[!colnames(data) %in% c("date",names(fm),"T_C")]
    if(length(missing.names)>0){
      warning(paste("no correction factor for",missing.names,collapse =" "))
    }

    #Kalibrierfunktion anwenden
    data[same.names] <-
      sapply(same.names,function(x){
        if(is.numeric(fm[[x]])){
          data[,x] + fm[[x]]
        }else{
          varname <- names(fm[[x]]$coefficients[2])
          new_df <- data[x]
          names(new_df) <- varname
          predict(fm[[x]],newdata=new_df)
        }
      })
    # data[same.names] <-
    #   sapply(same.names,function(x) predict(fm[[x]],newdata=data.frame(CO2_Dyn=data[[x]])))
  }#ende if korrektur
  return(data)
}#ende function
