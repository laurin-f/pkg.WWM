read_sweep <- function(filename = "freeSoil_anisotropy_sweep_3DS.txt",
                       format = "array") {
  sweep_lines <- readLines(paste0(comsolpfad,filename))
  
  n_DS <- stringr::str_extract(filename,pattern = "\\d+(?=DS)") %>% as.numeric()
  #Parameter die in der Datei gesweept wurden
  pars <- c(paste0("DS_",1:n_DS),"injection_rate")
  #Regular Expression für die unterschiedlichen Werte die die Parameter annehmen
  value_regexp <- "\\d+(\\.\\d+)?(E-\\d)?"
  
  #Spaltennahmen der sweep datei ausschneiden
  
  colnames_sweep <- str_extract_all(sweep_lines[9],paste0("(?<=% )r|z|",paste0(pars,"=",value_regexp,collapse=", ")),simplify = T)
  #ab Spalte 10 stehen die Werte in der Datei diese werden bei leerzeichen getrennt 
  sweep_mat <- str_split(sweep_lines[10:length(sweep_lines)],"\\s+",simplify = T)
  #die matrix als data.frame mit numerischen werden 
  sweep_wide <- as.data.frame(apply(sweep_mat,2,as.numeric))
  #Spaltennamen
  colnames(sweep_wide) <- colnames_sweep
  
  sweep_long <- tidyr::pivot_longer(sweep_wide[-1],cols=-(1),names_patter= paste0(paste(pars,collapse = "=(.*), "),"=(.*)"),values_to="CO2_mol_per_m3",names_to=pars) %>% 
    sapply(.,as.numeric) %>% as.data.frame()
  rm(list=c("colnames_sweep","sweep_mat","sweep_lines"))
  if(format == "long"){
    return(sweep_long)
  }else{
    
    sweep_long2 <- sweep_long
    for(i in paste0("DS_",1:n_DS)){
      sweep_long2[,i] <- paste0(i,"=",sweep_long[,i])
    }
    sweep_wide <- tidyr::pivot_wider(sweep_long2,names_from = matches("DS"),names_sep=", ",values_from=CO2_mol_per_m3) %>% as.data.frame()
    rm(sweep_long2)
    if(format == "wide"){
      
      return(sweep_wide)
    }
  } 
  if(format == "array"){
    
    mod_inj_rates <- unique(sweep_wide$injection_rate)
    sweep_colnames <- colnames(sweep_wide[-(1:2)])
    tiefen <- max(sweep_wide$z)-unique(sweep_wide$z)
    
    sweep_arr <- array(dim = c(nrow(sweep_wide)/2,ncol(sweep_wide)-2,2))
    for(i in 1:2){
      sweep_arr[,,i] <- as.matrix(subset(sweep_wide[,-(1)],injection_rate == unique(sweep_wide$injection_rate)[i]))[,-1]
    }
    
    dimnames(sweep_arr) <- list(tiefen,sweep_colnames,mod_inj_rates)
    return(sweep_arr)
  }
}
