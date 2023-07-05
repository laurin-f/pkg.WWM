

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
                    format="long",
                    corfac = T) {
  update_PP.db(table.name = table.name)
  
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
  
  if(corfac == T){
    load(file=paste0(datapfad_PP_Kammer,"P_corfac_2.RData"))
    
    for(i in 1:5){
      data[,paste0("P_",i)] <- data[,paste0("P_",i)]-P_corfac$P_mean[i]
    }
  }
  if(corfac == "P_corfac_date"){
    load(file=paste0(datapfad_PP_Kammer,"P_corfac_date.RData"))
    
    for(i in 1:5){
      corfac_i <- subset(P_corfac,id == i)
      cal_i <- approx(corfac_i$date,corfac_i$P_mean,xout = data$date,rule=2)$y
      data[,paste0("P_",i)] <- data[,paste0("P_",i)]-cal_i
    }
  }
  
  if(format == "long"){
    data <- tidyr::pivot_longer(data,
                                matches("PPC|P|P_filter"),
                                names_pattern = "(.+)_(\\d)",
                                names_to = c(".value","id"))
  }
  return(data)
}
