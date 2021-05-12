#pfade definieren
# #detach("package:pkg.WWM", unload = TRUE)
# hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
# samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/")
# comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
# metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
# metapfad_harth<- paste0(metapfad,"Hartheim/")
# metapfad_comsol<- paste0(metapfad,"COMSOL/")
# soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
# klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
# kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
# plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
# COMSOL_exepath <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
# COMSOL_progammpath <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
# #Packages laden
# library(pkg.WWM)
# packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr")
# ggopt()
# check.packages(packages)
#
# load(paste0(samplerpfad,"Hartheim_CO2.RData"))
# load(paste0(kammer_datapfad,"Kammer_flux.RData"))
#  mod_dates <- ymd_hm(c("2020-07-08 11:00","2020.07.08 12:00","2020.07.14 15:00"))
#  overwrite=T
#  read_all=F
#  n_DS=3
#  plot=T
#  offset_method = "glm"
#  #which optimization method should be used nelder or snopt
#' Title
#'
#' @param data
#' @param mod_dates
#' @param overwrite
#' @param read_all
#' @param n_DS
#' @param plot
#' @param offset_method
#' @param modelname
#'
#' @return
#' @export
#'
#' @examples
run_comsol <- function(data=data,
                       mod_dates,
                       overwrite=F,
                       read_all=T,
                       n_DS=3,
                       plot=F,
                       offset_method = "gam",
                       z_soil_cm = 150,
                       modelname = "Diffusion_freeSoil_optim_3DS",
                       file_suffix=NULL,
                       input_pars=NULL){

  data <- as.data.frame(data)
  n_DS_ch <- paste0(n_DS,"DS")

  #CO2 in tracer in mol pro m3
  if(!any(grepl("PressureActual_hPa",names(data)))){
    print("no Pressure Data available using 101.3 kPa as default")
    data$PressureActual_hPa <- 1013
  }
  if(!any(grepl("T_soil",names(data)))){
    print("no T_soil Data available using 20 °C as default")
    data$T_soil <- 20
  }
  data$D0 <- D0_T_p(data$T_soil,p_kPa = data$PressureActual_hPa/10,"m^2/s")
  data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)
  #negative werte auf null setzen
  data$CO2_mol_per_m3[data$tiefe == 0]<- 0
  data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0

  #model coordinates
  data$z <- z_soil_cm +data$tiefe
  data$r <- 0

  data_sub <- lapply(mod_dates,function(x) data[data$date==x,c("tiefe","date","CO2_mol_per_m3","inj_mol_m2_s","T_soil","PressureActual_hPa","CO2_ref")])
  names(data_sub) <- mod_dates

  ##############################################
  #Comsol ausführen
  ##############################################
  #
  date_chr <- format(mod_dates,"%m_%d_%H_%M")
  outfile_names <- paste0(modelname,"_",offset_method,"_",date_chr,".txt")

  if(!is.null(file_suffix)){
      outfile_names <- paste0(modelname,"_",offset_method,"_",date_chr,"_",file_suffix,".txt")
  }

  for(j in seq_along(data_sub)){
    sub_j <- data_sub[[j]]
    injection_rate <- sub_j$inj_mol_m2_s[sub_j$tiefe == -24.5]
    names(injection_rate) <- "injection_rate"

    #schreibe messungen in files die in COMSOL als Objective verwendet werden
    for(i in 1:7){
      write.table(sub_j[sub_j$tiefe == (1:7*-3.5)[i],"CO2_mol_per_m3"],paste0(metapfad_comsol,"dom",i,".csv"),col.names = F,row.names = F,sep=",")
    }
    if(is.null(input_pars)){
      input_pars_j <- injection_rate
    }else if(is.data.frame(input_pars)){
      input_pars_j <- cbind(input_pars,injection_rate)
    }else{
      input_pars_j <- c(input_pars,injection_rate)
    }
    #command der an commandline gesendet wird um comsolbatch.exe zu starten
    comsol_exe(modelname = modelname,
               outfile_raw="CO2_optim.txt",
               outfile_new=outfile_names[j],
               job="b1",
               overwrite=overwrite,
               input_pars=input_pars_j)

  }

  ##############################
  #COMSOL output
  ##############################
  #alle dateien mit der gewünschten methode und datum
  if(read_all == T){
    date_pattern <- "\\d{2}(_\\d{2}){2,3}"
    file_pattern <- paste(modelname,offset_method,date_pattern,sep="_")
    if(!is.null(file_suffix)){
      file_pattern <- paste(modelname,offset_method,date_pattern,file_suffix,sep="_")
    }
    outfile_names <- list.files(comsolpfad,pattern = paste0(file_pattern,".txt$"))
#
    mod_date_all_chr <- sort(unique(str_extract(outfile_names,date_pattern)))

    mod_date_all_chr_pad <- ifelse(nchar(mod_date_all_chr) == 8,paste0(mod_date_all_chr,"_00"),mod_date_all_chr)

    mod_dates_all <- ymd_hm(paste("2020",mod_date_all_chr_pad))
    mod_dates_all <-mod_dates_all[mod_dates_all %in% data$date]

    date_chr <- format(mod_dates_all,"%m_%d_%H_%M")
    outfile_names <- paste0(modelname,"_",offset_method,"_",date_chr,".txt")

    if(!is.null(file_suffix)){
      outfile_names <- paste0(modelname,"_",offset_method,"_",date_chr,"_",file_suffix,".txt")
    }
  }else{
    mod_dates_all <- mod_dates
  }
  outfiles <- paste0(comsolpfad,outfile_names)


    data_list <- lapply(mod_dates_all,function(x) data[data$date==x, ])
    names(data_list) <- as.character(mod_dates_all)

  F_Comsol <- data.frame(date=mod_dates_all,Fz=NA)

  ####################################
  #read loop
  #######################################
  for(j in seq_along(mod_dates_all)){

      CO2_optim <- read.csv(outfiles[j],skip=9,sep="",header=F)

      colnames(CO2_optim) <- c("r","z","CO2_mod_mol_m3",paste0("DS_",1:n_DS))
      CO2_mod <- CO2_optim[,1:3]
      best_DS <- CO2_optim[1,4:(n_DS+3)]


    obs_j <- data_list[[as.character(mod_dates_all)[j]]]
    obs_mod <- merge(obs_j,CO2_mod)

    if(n_DS > 1){
    if(n_DS == 4){
      schicht_grenzen <- seq(0,by=-7,length.out = n_DS)
      tiefen <- seq(-3.5,by=-7,length.out = n_DS)
    }
    if(n_DS == 3){
      schicht_grenzen <- c(0,-10.5,-21)
      tiefen <- c(-5.25,-15.75,-24.5)
    }
    if(n_DS == 2){
      schicht_grenzen <- c(0,-14)
      tiefen <- c(-7,-24.5)
    }
    schicht_untergrenzen <- c(schicht_grenzen[-1]+0.01,-z_soil_cm)
    DS_profil <- data.frame(DS=unlist(best_DS),tiefe=tiefen,top=schicht_grenzen,bottom=schicht_untergrenzen)
    for(i in 1:nrow(DS_profil)){
      tiefenID <- obs_mod$tiefe <= DS_profil$top[i] & obs_mod$tiefe > DS_profil$bottom[i]
      # DS_profil$DSD0_PTF_min[i] <- min(obs_mod$DSD0_PTF_min[tiefenID])
      # DS_profil$DSD0_PTF_max[i] <- max(obs_mod$DSD0_PTF_max[tiefenID])
      # DS_profil$DSD0_PTF[i] <- mean(obs_mod$DSD0_PTF[tiefenID])
      DS_profil$D0[i] <- mean(unlist(obs_mod[tiefenID,c("D0")]))
    }
    }

    if(plot == T){
      if(n_DS > 1){
      DS_profil_long <- tidyr::pivot_longer(DS_profil,!matches("DS|D0"),values_to = "tiefe")
      #DS_profil_long <- reshape2::melt(DS_profil,id=grep("DS|D0",colnames(DS_profil)),value.name="tiefe")
      par(mfrow=c(1,2))
      plot(obs_mod$CO2_mol_per_m3,obs_mod$tiefe,xlab=expression(CO[2]~"[mol m"^{-3}*"]"),ylab="depth [cm]",main=date_chr[j])
      lines(obs_mod$CO2_mod_mol_m3,obs_mod$tiefe,col=2)

      DS_profil_long <- DS_profil_long[order(DS_profil_long$tiefe),]
      plot(DS_profil_long$DS/DS_profil_long$D0,DS_profil_long$tiefe,ylim=range(obs_mod$tiefe),type="l",xlab="DS/D0",ylab="")

      par(mfrow=c(1,1))
      }else{
        plot(obs_mod$CO2_mol_per_m3,obs_mod$tiefe,xlab=expression(CO[2]~"[mol m"^{-3}*"]"),ylab="depth [cm]",main=paste(date_chr[j],"DSD0 =",round(best_DS/mean(obs_mod$D0,na.rm=T),2)))
      lines(obs_mod$CO2_mod_mol_m3,obs_mod$tiefe,col=2)
      }
      Sys.sleep(3)

      #dev.off()
    }
    slope_0_7cm <- glm(CO2_ref ~ tiefe, data= subset(obs_j,tiefe >= -7))#ppm/cm
    #plot(obs_j$tiefe,obs_j$CO2_ref)
    #abline(slope_0_7cm)
    dC_dz <- -slope_0_7cm$coefficients[2]

    dC_dz#ppm/cm
    #DS = -FZ * dz / dC

    dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units",p_kPa = unique(obs_j$PressureActual_hPa)/10,T_C = obs_j$T_soil[obs_j$tiefe == -3.5])#mol/m^3/cm
    if(n_DS > 1){
    Fz_mumol_per_s_m2 <- best_DS$DS_1  * dC_dz_mol * 100 * 10^6#m2/s * mol*10^6/m3/cm*100 = mumol/s/m2

    for(k in 1:n_DS){
      F_Comsol[F_Comsol$date == mod_dates_all[[j]],paste0("DSD0",k)] <- DS_profil$DS[k]/DS_profil$D0[k]
      F_Comsol[F_Comsol$date == mod_dates_all[[j]],paste0("DS",k)] <- DS_profil$DS[k]
    }
    }else{
      Fz_mumol_per_s_m2 <- best_DS  * dC_dz_mol * 100 * 10^6#m2/s * mol*10^6/m3/cm*100 = mumol/s/m2
      F_Comsol[F_Comsol$date == mod_dates_all[[j]],"DSD0"] <- best_DS/mean(obs_mod$D0,na.rm=T)
      F_Comsol[F_Comsol$date == mod_dates_all[[j]],"DS"] <- best_DS
    }
    F_Comsol$Fz[F_Comsol$date == mod_dates_all[[j]]] <- Fz_mumol_per_s_m2

  }
  return(F_Comsol)
}
