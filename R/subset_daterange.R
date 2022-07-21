#' Function that returns a subset of data within the given daterange
#'
#' @param data 
#' @param daterange 
#' @param datecol 
#'
#' @return
#' @export
#'
#' @examples
sub_daterange <- function(data,daterange,datecol="date") {
  data <- as.data.frame(data)
  id <- data[,datecol] >= min(daterange) & data[,datecol] <= max(daterange)
  return(data[id,])
}
#' Function that returns a vector with the IDs of the rows in data that lie in daterange
#'
#' @param data 
#' @param daterange 
#' @param datecol 
#'
#' @return
#' @export
#'
#' @examples
daterange_id <- function(data,daterange,datecol="date") {
  data <- as.data.frame(data)
  id <- data[,datecol] >= min(daterange) & data[,datecol] <= max(daterange)
  return(id)
}