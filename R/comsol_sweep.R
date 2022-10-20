#datelim <- lubridate::ymd_hms("2022-06-28 11:00:00 UTC", "2022-07-01 10:30:00 UTC")
#data <- pkg.WWM::read_sampler(datelim = datelim)
#library(dplyr)

#filename = "freeSoil_anisotropy_sweep_2DS.txt"
#' function to find DS values for observed CO2 Profiles that best fit modelled Profiles
#'
#' @param data 
#' @param intervall string describing the aggregation time intervall default \code{"30 mins"}
#' @param tracer_colname name of the tracer column 
#' @param filename name of the COMSOl sweep outputfile
#' @param plot if "DS_time" will plot DS timeline
#' @param byout intervall to interpolate extendend sweep matrix
#'
#' @return
#' @importFrom Hmisc approxExtrap
#' @export
#'
#' @examples
comsol_sweep <- function(data,
                         intervall = "30 mins",
                         tracer_colname = "CO2_tracer_drift",
                         filename = "freeSoil_anisotropy_sweep_3DS.txt",
                         DS_ratio = NULL,
                         plot = F,
                         extend  = F,
                         byout = 1e-7
) {
  
  
  n_DS <- stringr::str_extract(filename,pattern = "\\d+(?=DS)") %>% as.numeric()
  #Parameter die in der Datei gesweept wurden
  pars <- c(paste0("DS_",1:n_DS),"injection_rate")
  
  if(extend){
    #######################
    #extend matrix
    if(!file.exists(paste0(comsolpfad,stringr::str_remove(filename,".txt"),"_extend_arr.RData"))){
      extend_sweep_mat(filename,byout = byout)
    }
    load(paste0(comsolpfad,stringr::str_remove(filename,".txt"),"_extend_arr.RData"))
  }else{
    extend_arr <- read_sweep(filename,format = "array")
  }
  
  
  ########################
  #format data
  data_agg <- data %>% 
    mutate(date = lubridate::round_date(date,intervall),
           tracer_mol = ppm_to_mol(.data[[tracer_colname]],"ppm",T_C = T_soil),
           D0 = D0_T_p(T_soil,unit="m^2/s"),
           tiefe = abs(tiefe),
           inj_mol_m2_s = round(inj_mol_m2_s,3)) %>% 
    dplyr::filter(inj == 1 & !is.na(tracer_mol)) %>% 
    group_by(date,tiefe) %>% 
    summarise(across(everything(),mean))
  
  mod_dates <- unique(data_agg$date)
  
  
  #############################
  #interpolate  extend_arr for inj_rates
  mod_inj_rates <- dimnames(extend_arr)[[3]] %>% as.numeric()
  sweep_colnames <- dimnames(extend_arr)[[2]]
  DS_mod_ch <- stringr::str_extract_all(sweep_colnames,"(?<=DS_\\d=)\\d(\\.\\d+)?(E|e)-\\d+",simplify = T)
  DS_mod <- as.data.frame(apply(DS_mod_ch,2,as.numeric))
  colnames(DS_mod) <- stringr::str_subset(pars,"DS")
  
  
  
  tiefen <- dimnames(extend_arr)[[1]] %>% as.numeric()
  tiefen_df <- data.frame("tiefe"=tiefen)
  inj_rates <- unique(data_agg$inj_mol_m2_s)
  #array with sweep for each inj_rate
  sweep_arr <- array(dim = c(dim(extend_arr)[-3],length(inj_rates)))
  
  #loop to approx each value in array
  print(paste("interpolate",length(inj_rates),"inj_rates"))
  pb <-  txtProgressBar(min = 0, max = dim(extend_arr)[1], initial = 0,style=3) 
  for(j in 1:dim(extend_arr)[1]){
    for(k in 1:dim(extend_arr)[2]){
      sweep_arr[j,k,] <- Hmisc::approxExtrap(mod_inj_rates,extend_arr[j,k,1:2],inj_rates)$y
    }
    setTxtProgressBar(pb,j)
  }
  close(pb)
  dimnames(sweep_arr) <- list(tiefen,sweep_colnames,inj_rates)
  
  #############################################
  #df for DS
  DS_df <- data.frame(date=mod_dates)
  DS_df[,paste0("DS_",1:n_DS)]<- NA
  #DS_df[,paste0("DS_min_",1:n_DS)]<- NA
  #DS_df[,paste0("DS_max_",1:n_DS)]<- NA
  DS_df$RMSE <- NA
  
  columns <- c("date","tiefe","tracer_mol","inj_mol_m2_s","D0")
  
  ###############################################
  #loop for RMSE
  print("starting RMSE loop")
  pb <-  txtProgressBar(min = 0, max = length(mod_dates), initial = 0,style=3) 
  for(i in seq_along(mod_dates)){
    CO2_obs <- data_agg[data_agg$date == mod_dates[i],columns]
    inj_i <- unique(CO2_obs$inj_mol_m2_s)[1]
    
    if(!is.null(DS_ratio)){
      ratio_id <- abs(DS_mod$DS_2/DS_mod$DS_1 - DS_ratio[i,1]) < 0.1
      if(n_DS > 2){
        ratio_id2 <- abs(DS_mod$DS_3/DS_mod$DS_1 - DS_ratio[i,2]) < 0.1
        ratio_id <- ratio_id & ratio_id2
      }
      DS_mod_i <- DS_mod[ratio_id,]
      sweep_sub <- sweep_arr[,ratio_id,inj_rates==inj_i]
    }else{
      DS_mod_i <- DS_mod
      sweep_sub <- sweep_arr[,,inj_rates==inj_i]
    }
    
    CO2_obs <- CO2_obs[order(abs(CO2_obs$tiefe)),]
    CO2_obs <- merge(CO2_obs,tiefen_df,all=T)
    rmse <- apply(sweep_sub,2,RMSE,CO2_obs$tracer_mol,normalize = "sd")
    # DS_mat_ch <- str_extract_all(names(rmse),"(?<=DS_\\d=)\\d(\\.\\d+)?(E|e)-\\d+",simplify = T)
    # DS_mat <- as.data.frame(apply(DS_mat_ch,2,as.numeric))
    # 
    # colnames(DS_mat) <- str_subset(pars,"DS")
    
    #########################################
    #Bester RMSE
    ########################################
    best.fit.id <- which.min(rmse)
    best_rmse <- min(rmse)
    #uncert.id <- which(rmse <= rmse_th)
    #uncert.id <- which(rmse <= sort(rmse)[n_best])
    
    #Bester Parametersatz
    best_DS <- as.numeric(DS_mod_i[best.fit.id,])
    #uncertainty range
    #DS_range <- apply(DS_mod[uncert.id,],2,function(x) range(as.numeric(x)))
    #DS_range[is.infinite(DS_range)] <- NA
    
    #names(best_DS) <- colnames(DS_mat)
    DS_df[i,colnames(DS_mod)] <- best_DS
    #DS_df[i,paste0("DS_min_",1:n_DS)] <- DS_range[1,]
    #DS_df[i,paste0("DS_max_",1:n_DS)] <- DS_range[2,]
    
    DS_df[i,"D0"] <- mean(CO2_obs$D0,na.rm=T)
    DS_df[i,"RMSE"] <- best_rmse
    
    
    
    setTxtProgressBar(pb,i)
  }
  close(pb)
  
  DS_long <- tidyr::pivot_longer(DS_df,matches("DS"),names_to = c(".value","tiefe"),names_pattern = "(DS.*)_(\\d)")
  
  DS_long$DSD0 <- DS_long$DS / DS_long$D0
  # DS_long$DSD0_min <- DS_long$DS_min / DS_long$D0
  # DS_long$DSD0_max <- DS_long$DS_max / DS_long$D0
  DS_long$tiefe <- as.numeric(DS_long$tiefe)
  
  
  if(plot == "DS_time"){
    ggplot(DS_long)+
      geom_line(aes(date,DS,col=as.factor(tiefe)))
  }
  if(plot == "DS_RMSE"){
    ggplot(DS_long)+
      geom_point(aes(date,DS,col=RMSE,group=tiefe))+
      scale_color_viridis_c(limits = c(min(DS_long$RMSE),quantile(DS_long$RMSE,0.9)))
  }
  return(DS_long)
}


# library(raster)
# library(terra)
# r1 <- raster(extend_arr[,,1])
# r2 <- raster(extend_arr[,,2])
# s <- stack(r1,r2)
# 
# f <- function(x) approx(c(0.1,0.2), x, inj_rates, rule=2)$y
# # test <- f(s[1])
# tictoc::tic()
# x <- calc(s, f)
# terra::stackapply
# tictoc::toc()
# plot(x)
# test <- as.array(x)

