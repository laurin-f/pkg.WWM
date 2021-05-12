

#' Function to read data from LIMS sb
#'
#' @return
#' @export
#' @importFrom RODBC odbcConnect sqlQuery
#' @importFrom lubridate ymd
#' @examples read_LIMS()
read_LIMS<-function(){
channel <- odbcConnect("WINLIMS8")

data<-sqlQuery(channel, "SELECT mst.PROJEKT, mst.Probenart, mst.VAR_ID AS plot, mst.PROBENBEZ, mst.MST_ID, s.REMARKS, s.SAMPLE_NO, s.PN_DATUM, sp.NRESULT AS value, sp.PA_NAME AS variable, sp.PARAM_UNITS AS unit
                            FROM ((SAMPLE AS s
               INNER JOIN MSTVERWEIS AS mst ON s.MST_ID = mst.MST_ID)
               INNER JOIN SAMPLEPARAM AS sp ON s.SAMPLE_ID = sp.SAMPLE_ID)
               WHERE mst.PROJEKT = '1677DFG_Ma' AND sp.NRESULT IS NOT NULL")
odbcCloseAll()

data$date <- ymd(data$PN_DATUM)
data$tiefe<-(substr(data$PROBENBEZ,5,7))
data$tiefe[data$tiefe == "-WL"]<-80
data$tiefe<-as.numeric(data$tiefe)
data$value[data$variable == "CO2"] <- data$value[data$variable == "CO2"]*10^4
data$unit[data$variable == "CO2"] <-"ppm"
return(data)
}






