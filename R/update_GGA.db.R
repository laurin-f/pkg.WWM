#' @title Funktion um alle .zip dateien in einem Ordner incl. Unterordnern zu entpacken
#'
#' @description  Alle .zip dateien in einem Ordner incl. Unterordnern werden entpackt und im Unterordner unzipped gespeichert
#'
#' @param path the path that the zip files are in
#'
#' @return nothing returned
#'
#' @export
#'
#' @examples unzip.files("H:/FVA-Projekte/P01677_WindWaldMethan/Daten/Urdaten/GGA/")
unzip.files<-function(path){
  #zip files auflisten
  zips<-list.files(path,pattern=".zip$",
                   recursive = T,full.names = T)

  #Falls derunterordner unzipped nicht besteht, diesen erstellen
  if(!dir.exists(paste0(path,"unzipped/"))){
    dir.create(paste0(path,"unzipped/"))
  }
  #Schleife um zu testen ob die Dateien bereits im Ordner unzipped sind
  #und wenn nicht zu enpacken
  for(i in seq_along(zips)){
    #dateien der i-ten zip auflisten
    zip.list<-unzip(zips[i],list=T)

    #wenn die Datei nicht im unzipped Ordner steht
    if(!all(file.exists(paste0(path,"unzipped/",zip.list$Name)))){
      #unzippen
      cat(paste("unzipping file:",zip.list$Name,"\n"))
      unzip(zips[i],exdir = paste0(path,"unzipped"))
    }#ende if
  }#ende for
}#ende function

##########################
#function that copies all gga micro_f000x.txt files that are not zipped into the unzip folder
copy_f000x <- function(path){
  #alle dateien die f000x.txt im Namen enthalten auflisten
  files <- list.files(path,pattern="f\\d{4}.txt$",
                                 recursive = T,full.names = F)
  #die aus dem unzipped ordner weglassen
  files <- files[!grepl("unzipped/",files)]
  #den Ordner weglassen
  files_short <- stringr::str_remove(files,".+/")
  #alle schon im unzipped Ordner enthaltenen Datei auflisten
  unzipped <- list.files(paste0(path,"unzipped"),pattern="f\\d{4}.txt$")
  #die Neuen sind die die noch nicht im unzipped ordner vorkommen
  new <- !files_short %in% unzipped
  
  #die neuen Dateien in den unzipped Ordner kopieren
  file.copy(paste0(path,files[new]),paste0(path,"unzipped/",files_short[new]))

}

######################################
#read all gga and micro files
#' @title update the GGA database
#' @description all files that are not listed in the db_log folder are formated and imported into the GGA database
#' @param path the path that the unzipped and the db_log folder are in
#' @param sqlpath the path to the database
#'
#' @return nothing returned
#' @export
#' @import stringr
#' @import lubridate
#' @import RSQLite
#' @examples update_GGA.db()
update_GGA.db<-function(table.name=c("gga","micro"),path,sqlpath){

  #unzip Funktion anwenden
  unzip.files(path)
  #copy the not zipped files also into the unzipped folder
  copy_f000x(path)

  #Spaltennamen die uns interessieren
  cols<-c("X.CO2._ppm","X.CH4._ppm","X.H2O._ppm","X.CH4.d_ppm",
          "X.CO2.d_ppm","GasP_torr","GasT_C","AmbT_C")

  #SPaltennamen wie sie im Datensatz heißen sollen
  colnames<-stringr::str_replace_all(cols,c("(^X|\\.|_ppm$)"="","d"="dry"))

  #Query um tabelle in db zu erstellen
  createquery<-paste0("CREATE TABLE IF NOT EXISTS tablename (date_int INTEGER PRIMARY KEY",
                      paste(",",colnames,"REAL",collapse = ""),")")

  #sensornamen
  sensor_names<-table.name
  #schleife um db beider sensoren zu aktualisieren
  #beim neuen steckt im Dateinamen "micro" beim alten "gga"
  for(i in sensor_names){

    #namen der bereits in der db existierenden files laden
    if(file.exists(paste0(path,"db_log/",i,"_files.txt"))){
      files.old<-read.csv(paste0(path,"db_log/",i,"_files.txt"),stringsAsFactors = F)
    }else{
      files.old<-NULL
    }

    #alle Dateien des unzipped Ordners mit gga oder micro im Namen auflisten
    files<-list.files(paste0(path,"unzipped"),
                      pattern = i,full.names = F)
    #neue files auswählen
    files.new<-files[!files %in% files.old$x]

    #falls neue dateien dazugekommen sind
    if(length(files.new)>0){
      print(paste0("reading ",length(files.new), " new ",i," files"))
      #die aufgelisteten Dateien in eine Liste einlesen
      data.list<-lapply(paste0(path,"unzipped/",files.new), read.csv,skip=1)

      #Listen in einen Datensatz zusammenfügen
      data.new<-do.call("rbind",data.list)

      #listen löschen
      rm(data.list)

      #datum formatieren
      data.new$date<-parse_date_time(data.new$Time,"mdYHMS")

      #datum NAs entfernen
      data.new<-data.new[!is.na(data.new$date),]

      #gewünschte spalten auswählen
      data<-data.new[c("date",cols)]

      #date in date_int umbenennen
      colnames(data)<-c("date_int",colnames)
      #data_int als integer
      data$date_int<-as.integer(data$date_int)

      #duplikate entfernen
      data<-data[!duplicated(data$date_int),]
      
      #datum kleiner 2010 entfernen
      date2010 <- as.integer(ymd_h("2010.01.01 00"))
      data <- data[data$date_int > date2010,]

      #mit db verbinden
      con<-odbc::dbConnect(RSQLite::SQLite(),paste0(sqlpath,"GGA.db"))
      #falls tabelle in db nicht vorhanden wird sie hier erstellt
      DBI::dbExecute(con, stringr::str_replace(createquery,"tablename",i))

      print("appending to Database")
      #tabelle in db aktualisieren
      DBI::dbWriteTable(con,name=i,value=data,append=T)

      #disconnect Database
      DBI::dbDisconnect(con)

      if(!dir.exists(paste0(path,"db_log/"))){
        dir.create(paste0(path,"db_log/"))
      }

      #files Datei speichern
      write.csv(c(files),
                paste0(path,"db_log/",i,"_files.txt"),row.names = F)
    }#ende if
  }#ende for
}#ende function
