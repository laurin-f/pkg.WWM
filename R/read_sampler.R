#' Read sampler data from database
#'
#'
#' @param table.name name of the table in the database
#' @param format long or wide
#' @param ... other parameters parsed to read_db such as datelim or cols
#' (see help of read_db)
#'
#' @return
#' @export
#' @import stringr
#' @importFrom reshape2 melt
#' @examples datelim<-c("2020.02.19 12:05:00","2020.02.20 09:35:00")
#' data <- read_sampler("sampler1",datelim = datelim)

read_sampler <- function(table.name="sampler1u2",format="long", ...){

  data_wide<-read_db(db.name="dynament.db",
                     table.name=table.name, ...)
  if(nrow(data_wide) == 0){
    stop("no data in datelim")
  }
  if(grepl("sampler3",table.name)){
    temp_cols <- grep("temp",colnames(data_wide))

    data_roll <- sapply(data_wide[,temp_cols],zoo::rollapply,5,mean,fill=NA)
    data_wide[,temp_cols][which(abs(data_roll-data_wide[,temp_cols]) > 2,arr.ind = T)] <- NA
  }
  if(format=="long"){

    # data_long <- reshape2::melt(data_wide,id=which(!grepl("CO2",colnames(data_wide))),value.name="CO2")
    if(table.name == "sampler1u2"){
      data_long <- tidyr::pivot_longer(data_wide,contains("tiefe"),names_pattern="(CO2|temp)_tiefe(\\d)_(smp\\d)",names_to = c(".value","tiefe","sampler"))
      data_long <- tidyr::pivot_wider(data_long,names_from = sampler, values_from = CO2, names_prefix = "CO2_")
    }else{
    data_long <- tidyr::pivot_longer(data_wide,contains("tiefe"),names_pattern="(CO2|temp)_tiefe(\\d)",names_to = c(".value","tiefe"))
    }
    data_long$tiefenstufe <- as.numeric(data_long$tiefe)
    data_long$tiefe <- data_long$tiefenstufe * -3.5

    return(as.data.frame(data_long))
  }else{
    return(as.data.frame(data_wide))
  }
}
