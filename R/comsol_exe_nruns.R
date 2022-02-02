

#' Function to execute Comsol Modell with defined parameters
#'
#' @param modelname name of the Comsol Modell without .mph ending
#' @param outfile_raw name that COMSOL gives the outputfile 
#' @param COMSOL_progammpath path to COMSOL project file
#' @param COMSOL_exepath path to comsolbatch.exe
#' @param job name of the job that is defined in the COMSOL project file
#' @param overwrite should existing modellresults be overwritten or not 
#'
#' @return
#' @export
#'
#' @examples 
comsol_exe_nruns <- function(modelname,
                             data_list,
                             outfile_names,
                             outfile_raw = "CO2_optim.txt",
                             COMSOL_progammpath = COMSOL_progammpfad,
                             COMSOL_exepath = COMSOL_exepfad,
                             job = "b1",
                             overwrite = F,
                             nruns=10) {
  
  outfile_full <- paste0(comsolpfad,outfile_raw)
  par_file <- paste0(comsolpfad,"input_pars.txt")
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph -outputfile ",COMSOL_progammpath,modelname,"_solved.mph -job ",job," -paramfile ",par_file)
  
  if(file.exists(outfile_full)){
    file.remove(outfile_full)
  }
  
  if(overwrite == F){
    files <- list.files(comsolpfad)
    existing_files <- outfile_names %in% files
    if(any(existing_files)){
      cat(paste(paste(outfile_names[existing_files],collapse = "\n"),"\nalready exist set overwrite = T to replace them"))
    }
    data_list <- data_list[!existing_files]
    outfile_names <- outfile_names[!existing_files]
  }
  if(length(data_list) == 0){
    print("no new dates to be calculated")
  }else{
    print(paste("calculating",length(data_list),"dates"))
    for (j in seq(1,length(data_list),by=nruns)) {
      
      print(paste("j=",j))
      sub_j <- data_list[j:(j+nruns-1)]
      #injectionsrate zum Zeitpunkt j
      injection_range <- range(sapply(sub_j,function(x) x$inj_mol_m2_s[1],simplify = T))
      #for(l in 1:2){
      
      #injection_rate <- injection_range[l]
      injection_rate <- mean(injection_range)
      names(injection_rate) <- "injection_rate"
      write.table(t(injection_rate),file=par_file,row.names = F,quote = F,sep = " ")
      
      
        #schreibe messwerte in files die in COMSOL als Objective verwendet werden
        for (i in 1:7) {
          write.table(
            sub_j[[1]][sub_j[[1]]$tiefe == (1:7 * -3.5)[i], "CO2_mol_per_m3"],
            paste0(metapfad_comsol, "dom", i, ".csv"),
            col.names = F,
            row.names = F,
            sep = ","
          )
        }
      
      for(k in 1:nruns){
        print(paste("k=",k))
        
        jk <- j+k-1
        
        
        
        #string that is parsed to commandline
        if(k == 1){
          print("cmd")
          shell(cmd,translate=T,wait=F)
        }
        
        #outfile_names_l <- stringr::str_replace(outfile_names,".txt",paste0("inj",l,".txt"))
        #new name for outputfile with path
        outfile_jk <- paste0(comsolpfad,outfile_names[jk])
        
        while(!file.exists(outfile_jk)){
          if(file.exists(outfile_full)){
            for (i in 1:7) {
              write.table(
                sub_j[[k+1]][sub_j[[k+1]]$tiefe == (1:7 * -3.5)[i], "CO2_mol_per_m3"],
                paste0(metapfad_comsol, "dom", i, ".csv"),
                col.names = F,
                row.names = F,
                sep = ","
              )
            }
            
            file.rename(outfile_full,outfile_jk)
          }
          Sys.sleep(0.1)
          print("while loop")
        }
      }
      
      #commandline befehl ausführen
      
    }
  }
}





####################################
check_CPU2<-function(){
  #Liste erzeugen in der der Name und die CPU aller laufender Prozesse steht
  tasklist<-system(
    "wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",
    intern=T)
  #die Strings in der Liste bei mindestens zwei Leerzeichen auseinanderschneiden
  tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
  #die auseinandergeschnittenen Strings zu einer Matrix zusammenfügen
  tasks<-do.call("rbind",tasksplit)
  
  #abfragen ob in der Taskliste H1D_UNSC vorkommt
  if(length(grep("H1D_UNSC",tasks))>0){
    #while Schleife wiederholen solange die CPU von H1D größer als 0 ist
    while(length(which(tasks[grep("H1D_UNSC",tasks),2]>0))>0){
      #kurz warten
      Sys.sleep(1)
      #aktuelle Taskliste abfragen und wie gehabt formatieren
      tasklist<-system(
        "wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",
        intern=T)
      tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
      tasks<-do.call("rbind",tasksplit)
    }#ende while Schleife
  }#ende if length H1D >0
}#ende check_CPU Funktion

#CPU checken
check_CPU2()

