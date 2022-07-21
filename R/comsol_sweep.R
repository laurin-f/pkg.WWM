#datelim <- lubridate::ymd_hms("2022-06-28 11:00:00 UTC", "2022-07-01 10:30:00 UTC")
#data <- pkg.WWM::read_sampler(datelim = datelim)
library(dplyr)

comsol_sweep <- function(data,
                         intervall = "30 mins",
                         CO2_colname = "CO2_tracer_drift",
                         filename = "freeSoil_anisotropy_sweep_2DS.txt") {
  
  
  n_DS <- stringr::str_extract(filename,pattern = "\\d+(?=DS)") %>% as.numeric()
  #Parameter die in der Datei gesweept wurden
  pars <- c(paste0("DS_",1:n_DS),"injection_rate")
  
  if(!file.exists(paste0(comsolpfad,stringr::str_remove(filename,".txt"),"_extend_wide.RData"))){
    extend_sweep_mat(filename)
  }
  load(paste0(comsolpfad,stringr::str_remove(filename,".txt"),"_extend_wide.RData"))
  names(data)
  data_agg <- data %>% 
    mutate(date = lubridate::round_date(date,intervall),
           tracer_mol = ppm_to_mol(.data[[CO2_colname]],"ppm",T_C = T_soil)) %>% 
    filter(inj == 1) %>% 
    group_by(date,tiefe) %>% 
    summarise(across(everything(),mean))
  names(data_agg)
  columns <- c("date","tiefe","tracer_mol","inj_mol_m2_s")
  mod_dates <- unique(data_agg$date)
  
  extend_arr <- array(dim = c(nrow(extend_wide)/2,ncol(extend_wide)-2,2))
  for(i in 1:2){
    extend_arr[,,i] <- as.matrix(subset(extend_wide[,-(1)],injection_rate == unique(extend_wide$injection_rate)[i]))[,-1]
  }
  dimnames(extend_arr) <- list(150-unique(extend_wide$z),(colnames(extend_wide[-(1:2)])),unique(extend_wide$injection_rate))
  sweep_sub <- extend_arr[,,1]
  
  DS_df <- data.frame(date=mod_dates)
  DS_df[,paste0("DS_",1:n_DS)]<- NA
  
  pb <-  txtProgressBar(min = 0, max = length(mod_dates), initial = 0,style=3) 
  for(i in seq_along(mod_dates)){
    CO2_obs <- data_agg[data_agg$date == mod_dates[i],columns]
    inj_i <- unique(CO2_obs$inj_mol_m2_s)
    
    
    # 
    # xyz <- lapply(dimnames(extend_arr), as.numeric)
    # names(xyz) <- cols
    # 
    # xyz[[3]] <- inj_i
    # 
    # xyz_out <- expand.grid(xyz)
    # 
    # 
    # dimnames(extend_arr)
    # approx_vec <- e1071::interpolate(xyz_out, extend_arr)
    # 
    
    
    tictoc::tic()
    for(j in 1:dim(extend_arr)[1]){
      for(k in 1:dim(extend_arr)[2]){
        sweep_sub[j,k] <- approx(as.numeric(dimnames(extend_arr)[[3]]),extend_arr[j,k,1:2],inj_i)$y
      }
    }
    tictoc::toc()
    
    rownames(sweep_sub)
    CO2_obs <- CO2_obs[order(-CO2_obs$tiefe),]
    rmse <- apply(sweep_sub,2,RMSE,CO2_obs$tracer_mol)
    
    DS_mat_ch <- str_extract_all(names(rmse),"(?<=DS_\\d=)\\d(\\.\\d+)?(E|e)-\\d+",simplify = T)
    DS_mat <- as.data.frame(apply(DS_mat_ch,2,as.numeric))
    
    colnames(DS_mat) <- str_subset(pars,"DS")
    
    #########################################
    #Bester RMSE
    ########################################
    best.fit.id <- which.min(rmse)
    #good.fit.id <- which(rmse <= sort(rmse)[n_best])
    
    #Bester Parametersatz
    best_DS <- as.numeric(DS_mat[best.fit.id,])
    names(best_DS) <- colnames(DS_mat)
    DS_df[i,names(best_DS)] <- best_DS
    setTxtProgressBar(pb,i)
  }
  close(pb)
}


library(raster) 
r1 <- raster(extend_arr[,,1]) 
r2 <- raster(extend_arr[,,2]) 
s <- stack(r1,r2)
s <- stack(lapply(1:length(idx), function(x) setValues(r, runif(ncell(r))))) 
idx <- seq(as.Date("2000/1/1"), as.Date("2000/12/31"), by = "week") 
dr <- seq(as.Date("2000/1/1"), as.Date("2000/12/31"), by = "day") 

f <- function(x) approx(c(0.1,0.2), x, inj_i, rule=2)$y
# test <- f(s[1])
tictoc::tic()
x <- calc(s, f)
tictoc::toc()
plot(x)
test <- as.matrix(x)
