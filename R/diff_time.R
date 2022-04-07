#' function to make difftime easier
#'
#' @param time datetime vector
#' @param unit character 
#' @param out_class class of output
#' @param ... other params passed to difftime
#'
#' @return
#' @export
#'
#' @examples
diff_time <- function(time,unit = "secs",out_class  = "numeric", ...) {
  dt <- c(NA,difftime(time[-1],time[-length(time)],unit=unit, ...))
  as(dt,out_class)
}