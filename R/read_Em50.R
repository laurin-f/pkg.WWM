


#' @title update Em50 database
#' @description adds all the files in the datapfad Em50Data subdirectory to the Em50 database
#' @return nothing returned
#' @export
#' @importFrom readxl read_xlsx
#' @import stringr
#' @import lubridate
#' @import RSQLite
#' @importFrom dplyr pull full_join
#' @examples Em50.update.db()
update_Em50_data.db<-function(table.name="Em50",
                              path=datapfad_schaui){

  #wenn die Datei db_log.txt existiert dann ist
  #files.old ein string mit den Dateinamen in db_log.txt
  if(file.exists(paste0(path,"db_log.txt"))){
    files.old<-read.csv(paste0(path,"db_log.txt"),stringsAsFactors = F)
  #ansonsten ist files.old = NULL
  }else{
    files.old<-NULL
  }

  #die Spaltennamen für die Datenbank aus den Matedaten einlesen
  logger_colnames<-(readxl::read_xlsx(paste0(metapfad_schaui,"Gassammler_liste.xlsx"),sheet=3))
  #die Tiefen der unterschiedlichen Sensoren aus den Metadaten einlesen
  tiefen_liste<-readxl::read_xlsx(paste0(metapfad_schaui,"Gassammler_liste.xlsx"))

  #Die Dateinamen auflisten
  path_EM50 <- paste0(path,"EM50Data/")
  files<-list.files(paste0(path_EM50),".*",full.names = F)
  #files.new sind alle Dateinamen die nicht in files.old vorkommen
  files.new<-files[!files %in% files.old$x]

  #wenn files.new > 0 werden die entsprechenden Dateien eingelesen
  if(length(files.new)>0){

    print(paste("reading",length(files.new),"new files"))
    #Die neuen Dateien werden in eine Liste geschrieben
    data<-lapply(files.new,function(x){

      #die Logger ID (A-1, A-2, B-1 oder B-2) wird aus dem Dateinamen ausgelesen
      logger_id<-str_extract(x,"(A|B)-(1|2)(?=\\s\\d)")
      #die Spaltennamen der jeweiligen LoggerID werden aus den Metadaten gelesen
      colnames<- logger_colnames %>% dplyr::pull(logger_id) %>% na.omit()
      #die sensorID wird aus den Spaltennamen ausgeschnitten
      sensor_id<-str_extract(colnames,"[A-Z]*\\d+$")

      #die tiefe des Sensors wird anhand der SensorID aus den Metadaten gelesen
      tiefen<-sapply(sensor_id,function(x){
        tiefen_liste$`Tiefe [cm]`[tiefen_liste$id==x]})
      #PlotID wird aus der LoggerID ausgeschnitten
      plotAB<-str_extract(logger_id,"(A|B)")
      #neue Spaltennamen mit tiefe und PlotID werden zusammengefügt
      colnames_tiefe<-paste(colnames,tiefen,plotAB,sep="_")

      #dateityp wird ermittelt
      dateityp<-str_extract(x,"\\..*")
      #wenn es eine .xls datei ist
      if(grepl(".xlsx?",dateityp)){

        #wird die erste Reihe eingelesen
        line1<-readxl::read_excel(paste0(path_EM50,x),n_max=1)
        #und die Anzahl Spalten ermittelt
        n_cols<-ncol(line1)
        #dann wird die ganze datei ohne die erste Reihe eingelesen,
        #dabei werden die col_types entsprechend der Spaltenzahl angegeben
        #die erste Spalte ist date der rest numeric
        #dies wird gemacht da bei fehlwerten in den ersten Spalten der Spaltentyp
        #von read_xls nicht richtig erkannt wird
        data.x<-readxl::read_excel(paste0(path_EM50,x),skip=2,col_types = c("date", rep("numeric",n_cols-1)))
      # wenn die Datei eine .txt ist
      }else if(dateityp==".txt"){
        #wird sie mit angepasstem NA strin eingelesen
        data.x<-read.csv(paste0(path_EM50,x),sep="\t",dec=",",stringsAsFactors = F,na.strings = "* * * ")
        #Datum formatieren
        data.x$Measurement.Time <- mdy_hm(data.x$Measurement.Time)
        #volumetric water content wird in prozent umgerechnet
        #colnames + 1 da die datumsspalte im vektor colnames nicht vorkommt
        data.x[,grep("vwc",colnames)+1]<-data.x[,grep("vwc",colnames)+1]*100
      }

        #Damit die Dateien in die Datenbank eingefügt werden können müssen
        #die Spaltennamen und Anzehlen einheitliche sein:
        #wenn die Datei mehr Spalten hat als die colnames + datum
        if(ncol(data.x) > (length(colnames)+1)){
          #werden die übrigen Spalten weggelassen
          data.x<-data.x[,1:(length(colnames)+1)]
        # hat sie weniger Spalten
        }else if(ncol(data.x) < (length(colnames)+1)){
          #wird ermittelt wieviele Spalten fehlen
          cols_diff <- (length(colnames)+1) - ncol(data.x)
          #und die ensprechenden Spalten werden mit NAs angehängt
          data.x[tail(colnames_tiefe, 1:cols_diff)]<-NA
        }

      #Spaltennamen anpassen
      colnames(data.x)<-c("date_int",colnames_tiefe)

      #Bestimmen in welchen Reihen mindestens eine Spalte nicht NA ist
      not_only_NA <- apply(data.x[,-1],1,function(x) any(!is.na(x)))
      #nur diese Reihen auswählen
      data.x<-data.x[not_only_NA,]

      return(data.x)}
      )#ende lapply

    #loggerIDs aller neuen Dateien
    logger_ids<-str_extract(files.new,"(A|B)-(1|2)(?=\\s\\d+[A-z]+\\d+)")

    #Alle dateien die vom gleichen Logger stammen mit rbind zusammenfügen
    data_rbind<-lapply(unique(logger_ids),function(x){
      #ID der lsitenelemente einer bestimmten loggerID
      listen_id<-which(logger_ids==x)
      #Falls mehrere Listenelemente zu gleichen Logger gehören
      if(length(listen_id)>1){
        #diese Listenelemente
        #mittels rbind zusammenfügen
        do.call("rbind",data[listen_id])
      }else{
        #falls nur eine datei der LoggerID vorhanden ist diese auswählen
        data[[listen_id]]
      }
    })

    #die Liste mit den zusammengefügten Dateien der Logger zu einem gesamt Datensatz joinen
    data_wide<-Reduce(dplyr::full_join,data_rbind)
    #und nach Datum sortieren
    data_wide<-data_wide[order(data_wide$date_int),]

    #verbindung mit der Datenbank aufnehmen
    con<-odbc::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"Em50_data.db"))

    print("appending to Database")
    #Dateien zur Datenbank hinzufügen
    dbWriteTable(con,table.name,data_wide,append=T)
    #Verbindung zu Datenbank trennen
    dbDisconnect(con)

    #db_log.txt aktualisieren
    write.csv(files,
              paste0(path,"db_log.txt"),row.names = F)
  }#ende if files.new > 0
}#ende Funktion


#' @title Function to read Data from Em50Data.db
#' @description reads in Data from Em50Data.db and formats it if wanted to long format
#' @param format either \code{"long"} (default) or \code{"wide"}
#' @param ... other parameters parsed to read_db such as datelim or cols
#' (see help of read_db)
#'
#' @return
#' @export
#' @importFrom tidyr gather
#' @import stringr
#' @import lubridate
#' @import RSQLite
#' @examples
#' read.Em50("long",datelim = c("2020.01.01 00:00:00","2021.01.01 00:00:00"))
read.Em50<-function(format="long", ...){
  #daten mit read_db aus datenbank laden
  data_wide<-read_db(db.name="Em50_data.db",
                     table.name="Em50", ...)
  #wenn longformat gewünscht ist...
  if(format=="long"){
    #ins long-format bringen
    data_long<-tidyr::pivot_longer(data_wide,-date,names_pattern="(.+)_(.+)_(-?\\d+)_(A|B)",names_to = c(".value","key","tiefe","plot"))
    # data_long<-tidyr::gather(data_wide,"key","value",-date)
    # 
    # #tiefe unit und plotID aus key ausschneiden
    # data_long$tiefe<-as.numeric(str_extract(data_long$key,"-?\\d+(?=_[A|B]$)"))
    # data_long$unit<-str_extract(data_long$key,"^[A-Z|a-z]+")
    # data_long$plot<-str_extract(data_long$key,"[A|B]$")

    return(data_long)
  }else{
    return(data_wide)
  }
}



