#' Function to change units of a vector
#'
#' @param x vector
#' @param unit_in unit of input vector as character
#' @param unit_out output unit as character
#' @param class_out class of output either \code{"numeric"} or \code{"units"}
#'
#' @return
#' @export
#'
#' @examples
change_unit <- function(x,
                        unit_in=NULL,
                        unit_out,
                        class_out="numeric"){
  if(!is.null(unit_in)){
    x_unit_in <- units::set_units(x, unit_in, mode="standard")
  }else{
    if(class(x) != "units"){
      stop("unit_in has to be defined for non unit input")
    }
    x_unit_in <- x
  }
  x_unit_out <- units::set_units(x_unit_in, unit_out, mode="standard")
  return(as(x_unit_out,class_out))
}
