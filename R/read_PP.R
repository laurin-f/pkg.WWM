

#' Function to read Pressure data from db
#'
#' @param datelim 
#' @param cols 
#'
#' @return
#' @export
#'
#' @examples
read_PP <- function(datelim=NULL, #Zeitrahmen der geladen werden soll
                    cols="*",
                    table.name="PP_chamber",
                    format="long") {
  update_PP.db()
  
  #db verbinden
  con<-odbc::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"PP.db"))
  
  #date_int wird als datum formatiert
  query<-paste0("SELECT ",cols," FROM ",table.name)
  
  #falls datelim angegeben wurde wird es hier als integer in die query aufgenommen
  if(!is.null(datelim)){
    from_to <- date_ms_as_int(ymd_hms(datelim))
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
  data$date<-date_ms_as_int(data$date_int)
  #date_int weglassen
  data<-data[,colnames(data)!="date_int"]
  
  #data[,paste0("P_",1:6)] <- V_to_Pa(data[,paste0("P_",1:6)])
  
  
  if(format == "long"){
    data <- tidyr::pivot_longer(data,
                                     matches("PPC|P|P_filter"),
                                     names_pattern = "(.+)_(\\d)",
                                     names_to = c(".value","id"))
  }
  return(data)
}
