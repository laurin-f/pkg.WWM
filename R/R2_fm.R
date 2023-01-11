#der R2 der regression wird bestimmt
#Funktion um R2 aus fm zu bestimmen
#' Title
#'
#' @param fm 
#'
#' @return
#' @export
#'
#' @examples
R2_fm <- function(fm){
  R2 <- 1-fm$deviance/fm$null.deviance
  return(R2)
}