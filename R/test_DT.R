#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
test_DT <- function(data){
  data$messid <- seq_along(data)
  T_DT <- data.table::setDT(data)
  T_DT <- T_DT[!is.na(messid),lapply(.SD,mean,na.rm=T),by = messid,.SDcol = T_deg]
  T_df <- as.data.frame(T_DT)
  return(T_df)
}

