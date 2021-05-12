read_comsol <- function(filename,colnames) {
  df <- read.csv(paste0(comsolpfad,filename),skip=9,sep="",header=F)
  colnames(df) <- colnames
  return(df)
}
