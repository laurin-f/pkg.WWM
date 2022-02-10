interp_comsol_inj <- function(...){
  out_list <- list()
  for(i in c("min","max")){
  out_list[[i]] <- run_comsol_nruns(inj_fun = i, file_suffix = i, ...)
    if(comsolbatch_CPU()){
      system("taskkill /IM comsolbatch.exe /F",show.output.on.console=T)
    }
  }
  return(out_list)
}
