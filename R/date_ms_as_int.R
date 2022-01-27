#' Function to convert a date with 10Hz to Integer 
#'
#' @param date input date with 10 Hz
#'
#' @return date_int as Interger in 100ms since 2022-01-27 00:00:00.0
#' @export
#'
#' @examples
date_ms_as_int <- function(date) {
  ref_date <- strptime("2022-01-27 00:00:00.0","%Y-%m-%d %H:%M:%OS")
  
  if(class(date)[1] %in% c("POSIXlt","POSIXct")){
    date_int <- round(as.numeric(difftime(date,ref_date,units="secs"))*10)
    return(date_int)
  }else if(class(date) %in% c("numeric","integer")){
    date_Posix <- ref_date + date/10
    return(date_Posix)
  }
  
}
