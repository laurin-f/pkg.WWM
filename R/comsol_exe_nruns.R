

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
                             #inj_fun = "mean",
                             nruns=10) {
  
  outfile_full <- paste0(comsolpfad,outfile_raw)
  probe_table <- paste0(comsolpfad,"Probe_table.txt")
  probe <- F
  break_for <- F
  #par_file <- paste0(comsolpfad,"input_pars.txt")
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph -outputfile ",COMSOL_progammpath,modelname,"_solved.mph -job ",job)#," -paramfile ",par_file)
  
  if(file.exists(outfile_full)){
    file.remove(outfile_full)
  }
  
  if(overwrite == F){
    files <- list.files(comsolpfad)
    existing_files <- outfile_names %in% files
    if(any(existing_files)){
      cat(paste(paste(outfile_names[existing_files],collapse = "\n"),"\nalready exist set overwrite = T to replace them\n"))
    }
    data_list <- data_list[!existing_files]
    
    outfile_names <- outfile_names[!existing_files]
  }
  
  if(length(data_list) == 0){
    print("no new dates to be calculated")
  }else{
    print(paste("calculating",length(data_list),"dates"))
    if(length(data_list) > 1){
      pb <-  txtProgressBar(min = 1, max = length(outfile_names), initial = 1,style=3)
    }else{
      pb <-  txtProgressBar(min = 0, max = length(outfile_names), initial = 0,style=3)
    }
    if(comsolbatch_CPU()){
      cat("\n")
      system("taskkill /IM comsolbatch.exe /F",show.output.on.console=T)
    }
    
    for (j in seq(1,length(data_list),by=nruns)) {
      
      
      sub_j <- data_list[j:(j+nruns-1)]
      
      #injectionsrate zum Zeitpunkt j
      # injection_range <- range(sapply(sub_j,function(x) x$inj_mol_m2_s[1],simplify = T))
      # #for(l in 1:2){
      # 
      # #injection_rate <- injection_range[l]
      # fn <- get(inj_fun,mode="function")
      # injection_rate <- fn(injection_range)
      # names(injection_rate) <- "injection_rate"
      # write.table(t(injection_rate),file=par_file,row.names = F,quote = F,sep = " ")
      # 
      
      
      #schreibe messwerte in files die in COMSOL als Objective verwendet werden
      #for (i in 1:7) {
        cols <- c("CO2_mol_per_m3","inj_mol_m2_s","date_int")
        
        write.table(
          cbind(t(sub_j[[1]][order(-sub_j[[1]]$tiefe), cols[1]]),
              unique(sub_j[[1]][,cols[2:3]])),
          paste0(metapfad_comsol, "GlobalLeastSquares.csv"),
          col.names = F,
          row.names = F,
          sep = ","
        )
      #}
      
      for(k in 1:nruns){
        jk <- j+k-1
        setTxtProgressBar(pb,jk)
        
        
        #string that is parsed to commandline
        if(k == 1){
          shell(cmd,translate=T,wait=F)
        }
        
        #outfile_names_l <- stringr::str_replace(outfile_names,".txt",paste0("inj",l,".txt"))
        #new name for outputfile with path
        if(jk <= length(outfile_names)){
          if(break_for & k <= nruns){
            #break
            break_for <- F
            shell(cmd,translate=T,wait=F)
            cat(paste("\nbreak_for j=",j,"k=",k))
          }
          #cat(paste("date ",jk,": ",outfile_names[jk]))
          outfile_jk <- paste0(comsolpfad,outfile_names[jk])
          
          #while(!file.exists(outfile_jk) ){
          counter <- 1
          while(!file.exists(outfile_full)){
            Sys.sleep(1.1)
            counter <- counter + 1
            #if(counter > 2){
            if(!comsolbatch_CPU()){
              break_for <- T
              cat(paste0("\nbreaking at counter=",counter))
              break
            }
            #}
            if(file.exists(probe_table)){
              if(probe == F){
                #if(k < nruns & !is.null(sub_j[[k +1]]$tiefe)){
                if(jk < length(outfile_names)){
                  write.table(
                    cbind(t(sub_j[[k + 1]][order(-sub_j[[k + 1]]$tiefe), cols[1]]),
                          unique(sub_j[[k + 1]][,cols[2:3]])),
                    paste0(metapfad_comsol, "GlobalLeastSquares.csv"),
                    col.names = F,
                    row.names = F,
                    sep = ","
                  )
                  probe <- T
                }
              }
              
            }
          }
          if(file.exists(outfile_full)){
            
            Sys.sleep(0.15)
            file.rename(outfile_full,outfile_jk)
            
            # outlines <- readLines(outfile_jk)
            # outlines[1] <- paste("injection_rate",injection_rate)
            # writeLines(outlines,outfile_jk)
            
            if(file.exists(probe_table)){
              file.remove(probe_table)
              probe <- F
            }
          }
          
        }else{
          if(comsolbatch_CPU()){
            cat("\n")
            system("taskkill /IM comsolbatch.exe /F",show.output.on.console=T)
          }
          
        }#if jk
        if(k == nruns){
          if(comsolbatch_CPU()){
            cat("\n")
            system("taskkill /IM comsolbatch.exe /F",show.output.on.console=T)
          }
        }
      }# for k
    }# for j
    close(pb)
    cat("finished comsol loop")
  }
}





####################################
#' Title
#'
#' @return
#' @export
#'
#' @examples
comsolbatch_CPU<-function(){
  #Liste erzeugen in der der Name und die CPU aller laufender Prozesse steht
  tasklist<-system(
    "wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",
    intern=T)
  #die Strings in der Liste bei mindestens zwei Leerzeichen auseinanderschneiden
  tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
  #die auseinandergeschnittenen Strings zu einer Matrix zusammenfügen
  tasks<-do.call("rbind",tasksplit)
  
  T_F <- any(grepl("comsolbatch",tasks))
  return(T_F)
}#ende check_CPU Funktion


