#####################################################
#update dynament.db funktion
#' @title update the dynament database
#' @description all files that are not listed in the db_log file are formated and imported into the dynament database
#' @param table.name name of the table in the databese that shall be updated
#' @return nothing returned
#' @export
#' @import stringr
#' @import lubridate
#' @import RSQLite
#' @importFrom rquery natural_join
#' @examples update_dynament.db("dynament_test")
update_dynament.db<-function(table.name="dynament_test"){

  if(grepl("sampler3",table.name)){
    file_pattern <- ".TXT"
    path <- arduinopfad
  }else{
    file_pattern <- ".csv"
    path <- dynpfad
  }
  #namen der bereits in der db existierenden files laden
  if(file.exists(paste0(path,"db_log_",table.name,".txt"))){
    files.old<-read.csv(paste0(path,"db_log_",table.name,".txt"),stringsAsFactors = F)
  }else{
    files.old<-NULL
  }

  #alle csv-Dateien aus dem dynament Ordner auflisten
  dyn.files<-list.files(path,file_pattern,full.names = F)


  #neue files auswählen
  files.new<-dyn.files[!dyn.files %in% files.old$x]

  ###################################################################
  #wenn neue files vorhanden sind...
  if(length(files.new)>0){
    print(paste("loading",length(files.new),"files"))
    #neue files laden
    if(grepl("sampler3",table.name)){
      dyn.list <- lapply(paste0(path,files.new),read.csv,sep=";",stringsAsFactors = F,na.strings = c("NA","ovf","0.00","-0.00","-250.00"))


      dyn.list.sub <- dyn.list

      dyn <- do.call(rbind,dyn.list)

      
      char_cols <- which(sapply(dyn[-1],class)== "character")+1
      
      dyn[,char_cols] <- sapply(dyn[,char_cols],as.numeric)

      CO2_cols <- grep("CO2",colnames(dyn))
      dyn[,CO2_cols][dyn[,CO2_cols] < 0] <- NA


      dyn[dyn < -20] <- NA
      dyn[dyn > 7000] <- NA

      
      dyn$date <- as.numeric(lubridate::ymd_hms(dyn$date))
      dyn <- dyn[!is.na(dyn$date),]
      
      colnames(dyn) <- stringr::str_replace(colnames(dyn),"date","date_int")
      
      if(!grepl("raw",table.name)){
      load(paste0(metapfad_dyn,"korrektur_fm.RData"))#,envir = .GlobalEnv)
      names(fm) <- stringr::str_replace(names(fm),"_sampler3","")
      same.names <- names(fm)[names(fm) %in% colnames(dyn)]

      #Korrekturfaktoren anwenden
      dyn[same.names] <-
        sapply(same.names,function(x){
          if(is.numeric(fm[[x]])){
            dyn[,x] + fm[[x]]
          }else{
            varname <- names(fm[[x]]$coefficients[2])
            new_df <- dyn[x]
            names(new_df) <- varname
            predict(fm[[x]],newdata=new_df)
            }
          })
      }
    }else{
      #if table.name != "sampler3"
      dyn.list<-lapply(paste0(path,files.new), read.csv,skip=1,stringsAsFactors=F)

      #spaltennamen der neuen files
      if(table.name=="dynament_test"){
        db.colnames<-c("date_int",paste0("CO2_Dyn_",sprintf("%02d",0:21)),"T_C")
      }else if(table.name == "sampler1"){
        db.colnames <- c("date_int",paste0("CO2_tiefe",0:7),"T_C")
      }else if(table.name == "sampler1u2"){
        db.colnames <- c("date_int",paste0("CO2_tiefe",rep(0:7,2),"_smp",rep(1:2,each=8)),"T_C")
      }else if(!is.na(str_extract(table.name,"sampler"))){
        db.colnames <- c("date_int",paste0("CO2_tiefe",0:7))
      }else{
        stop('table.name has to be either \n "dynament_test" \nor \n "samplerX"')
      }



      dyn.colnames<-lapply(paste0(path,files.new), read.csv,nrows=1,header=F,stringsAsFactors=F)

      #die ersten beidne Spalten sind daymonthyear und HourMinSec
      for(i in seq_along(dyn.colnames)){
        dyn.colnames[[i]][1:2]<-c("dmy","HMS")
      }

      ##########################################################
      if(table.name=="dynament_test"){
        #ids der Datumsspalten und aller CO2_Dyn Spalten
        dyn.colnames <- lapply(dyn.colnames, function(x) as.data.frame(t(str_replace(x,"Hygro_S3_Temperatur", "T_C")),stringsAsFactors = F))
        col.ids<-lapply(dyn.colnames,function(x)
          x %in% c("dmy","HMS",db.colnames))
      }

      #########################################################
      #wenn die Tabelle samplerx gewählt wurde
      if(table.name=="sampler1u2"){

        col.ids<-lapply(dyn.colnames,function(x) {
          #die Spaltennamen aus der .csv werden so formatiert das sie der Namen in der .db enstprechen
          #beim sampler werden dabei keine Dyn Nummern abgelegt
          colnames_x_formatiert <- str_replace_all(x,c("_Dyn\\d+"="","dpth"="tiefe","Hygro_S3_Temperatur" = "T_C"))
          #die spaltennamen der db mit dem tablename zusammengefügt entsprechen nun den Spaltennamen der csv
          ids.temp <- colnames_x_formatiert %in% paste(db.colnames,sep="_")
          ids.temp[1:2]<-T
          ids.temp
        })
      }else if(!is.na(str_extract(table.name,"sampler"))){

        col.ids<-lapply(dyn.colnames,function(x) {
          #die Spaltennamen aus der .csv werden so formatiert das sie der Namen in der .db enstprechen
          #beim sampler werden dabei keine Dyn Nummern abgelegt
          colnames_x_formatiert <- str_replace_all(x,c("_Dyn\\d+"="","dpth"="tiefe","smp"="sampler","Hygro_S3_Temperatur" = "T_C_sampler1"))
          #die spaltennamen der db mit dem tablename zusammengefügt entsprechen nun den Spaltennamen der csv
          ids.temp <- colnames_x_formatiert %in% paste(db.colnames,table.name,sep="_")
          ids.temp[1:2]<-T
          ids.temp
        })
      }


      #nur die relevanten Spalten aller tabellen der liste auswählen
      dyn.list.kurz<-dyn.list
      for(i in seq_along(dyn.list)){
        dyn.list.kurz[[i]] <- dyn.list[[i]][,col.ids[[i]]]
        #spaltennamen übernehmen
        colnames(dyn.list.kurz[[i]])<-dyn.colnames[[i]][,col.ids[[i]]]
      }

      #anzahl spalten aller listenelemente
      col.nr<-sapply(dyn.list.kurz,ncol)
      #nur listenelemente mit mehr als 2 spalten übernehmen also mindestens eine CO2_Dyn spalte
      dyn.list.sub<-dyn.list.kurz[which(col.nr > ifelse(table.name=="dynament_test",2,8))]

      if(!is.na(str_extract(table.name,"sampler"))){

        load(paste0(metapfad_dyn,"korrektur_fm.RData"))#,envir = .GlobalEnv)
        for(i in seq_along(dyn.list.sub)){
          data<-dyn.list.sub[[i]]

          #CO2 von mV in ppm umrechnen
          CO2_cols <- grep("CO2", colnames(data))
          data[, CO2_cols][which(data[,CO2_cols] > 2.7,arr.ind = T)]<-NA
          #data[, CO2_cols][which(data[,CO2_cols] < 0.25,arr.ind = T)]<-NA
          data[,CO2_cols] <- (data[,CO2_cols]-0.4)/1.6*5000
          data[, CO2_cols][which(data[,CO2_cols] <= 0,arr.ind = T)]<-NA
          colnames.data.old <- colnames(data)
          colnames(data)<-str_replace(colnames(data), "_dpth\\d_smp\\d$", "")
          ################hiervor muss noch korrigiert werden
          names(fm) <- str_replace(names(fm),"Dyn_","Dyn")
          same.names <- names(fm)[names(fm) %in% colnames(data)]

          #Korrekturfaktoren anwenden
          data[same.names] <-
            sapply(same.names,function(x) predict(fm[[x]],newdata=data.frame(CO2_Dyn=data[[x]])))

          if(table.name == "sampler1u2"){
            colnames(data)<-str_replace_all(colnames.data.old,c("(_Dyn_?\\d{2})"="","dpth"="tiefe", "Hygro_S3_Temperatur" = "T_C"))
          }else{
            #Spaltennamen anpassen
            colnames(data)<-str_replace_all(colnames.data.old,c("(_Dyn_?\\d{2})|(_smp\\d$)"="","dpth"="tiefe", "Hygro_S3_Temperatur" = "T_C"))
          }
          dyn.list.sub[[i]] <- data
        }
        #rm(fm)
      }

      #bei jedem listenelement die nicht vorkommenden spalten als NAs hinzufügen
      for(i in seq_along(dyn.list.sub)){
        add<-db.colnames[!db.colnames %in% colnames(dyn.list.sub[[i]])]
        dyn.list.sub[[i]][add]<-NA
      }

      #gekürtzte liste in eine Tabelle
      dyn<-do.call("rbind",dyn.list.sub)

      if(!is.null(dyn)){
        #datum formatieren
        dyn$date_int<-as.numeric(dmy_hms(paste(dyn$dmy,dyn$HMS)))
        #spalten dmy und HMS weglassen
        dyn<-dyn[db.colnames]
      }
      if(table.name=="dynament_test"){
        #CO2 von mV in ppm umrechnen
        CO2_cols <- grep("CO2", colnames(dyn))
        #9999 werte zu NA
        dyn[, CO2_cols][which(dyn[,CO2_cols] > 10,arr.ind = T)]<-NA
        #0.2 Werte zu NA
        dyn[, CO2_cols][which(dyn[,CO2_cols] < 0.25,arr.ind = T)]<-NA
        #CO2 von mV in ppm umrechnen
        dyn[,CO2_cols] <- (dyn[,CO2_cols]-0.4)/1.6*5000
      }

    }#ende if sampler 3

    #are there date duplicates
    date_duplicate <- duplicated(dyn$date_int)

    #if yes then the cases with duplicates are joined
    #replacing NA values of the duplicated rows with the values in the corresponding cases
    if(any(date_duplicate)){
      dyn <- rquery::natural_join(dyn[date_duplicate,],dyn[!date_duplicate,],by="date_int", jointype = "FULL")
    }

    if(is.null(dyn)){
      print(paste("no new files for",table.name))
    }else{
      #db verbinden
      con<-RSQLite::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"dynament.db"))
      #falls tabelle in db nicht vorhanden wird sie hier erstellt
      createquery<-paste0("CREATE TABLE IF NOT EXISTS ",table.name," (date_int INTEGER PRIMARY KEY",
                          paste(",",colnames(dyn)[-1],"REAL",collapse = ""),")")
      DBI::dbExecute(con, createquery)

      print(paste("appending",length(dyn.list.sub),"files to Database"))

      #Primary Key dopplungen checken
      db_duplicates <- DBI::dbGetQuery(con,paste("SELECT * FROM",table.name,"WHERE date_int >=",min(as.numeric(dyn$date_int)),"AND date_int <=",max(as.numeric(dyn$date_int))))

      #falls im zeitraum der neuen messungen bereits werte in der db sind
      if(nrow(db_duplicates) > 0){
        print("date duplicates")
        #die neuen werte aufteilen in den gedoppelten zeitraum und den der nicht in der db auftaucht
        dyn_duplicate <- dyn[dyn$date_int %in% db_duplicates$date_int,]
        dyn <- dyn[!dyn$date_int %in% db_duplicates$date_int,]

        #dopplungen in db mit neuen werten joinen und NAs überschreiben
        db_duplicates_join <- rquery::natural_join(db_duplicates,dyn_duplicate,by="date_int", jointype = "FULL")

        #die werte in string für die Query schreiben
        values_NA <- paste(apply(db_duplicates_join,1,paste,collapse = ", "),collapse="), (")
        values_NULL <- stringr::str_replace_all(values_NA, c("NA"="NULL"))
        replace_query <- paste0("REPLACE INTO ",table.name," (",paste(colnames(db_duplicates_join),collapse = ", "),") VALUES (",values_NULL,");")
        #cases in db ersetzten
        rs <- DBI::dbSendQuery(con,replace_query)
        DBI::dbClearResult(rs)
      }


      #Database aktualisieren
      DBI::dbWriteTable(con,name=table.name,value=dyn,append=T)

      #disconnect Database
      dbDisconnect(con)
    }
    #files Datei speichern
    write.csv(c(dyn.files),
              paste0(path,"db_log_",table.name,".txt"),row.names = F)
  }else{#ende if files.new
    print(paste(table.name,"is up to date"))
  }
}#ende function


#funktionen anwenden
# update_GGA.db()
# update_dynament.db()
# update_dynament.db(table.name = "sampler1")

#test<-dbGetQuery(con,"SELECT * FROM dynament_test")

#con<-odbc::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"dynament.db"))
#odbc::dbRemoveTable(con,"sampler3")
