#pfade definieren

#hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"


hauptpfad <- paste0(getwd(),"/../../")


#Urdaten
ggapfad<-paste0(hauptpfad,"Daten/Urdaten/GGA/")
dynpfad<-paste0(hauptpfad,"Daten/Urdaten/Dynament/")
datapfad_schaui<-paste0(hauptpfad,"Daten/Urdaten/Schauinsland/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
arduinopfad<-paste0(hauptpfad,"Daten/Urdaten/Arduino/")
PP_datapfad<-paste0(hauptpfad,"Daten/Urdaten/PP_Arduino/")

#aufbereitete Daten
sqlpfad<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")

#metadaten
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")
metapfad_dyn<- paste0(hauptpfad,"Daten/Metadaten/Dynament/")
metapfad_tracer<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
metapfad_schaui<-paste0(hauptpfad,"Daten/Metadaten/Schauinsland/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")

#special
COMSOL_exepfad <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
