#' Compute RootMeanSquareError
#'
#' @param mod modelled data
#' @param obs observed data
#'
#' @return RMSE
#' @export
#'
#' @examples RMSE(CO2_mod,CO2_obs)
RMSE <- function(mod,obs,normalize=F){
  rmse <- sqrt(mean((mod-obs)^2,na.rm = T))
  if(normalize =="mean"){
    rmse <- rmse/mean(obs,na.rm = T)
  }
  if(normalize =="sd"){
    rmse <- rmse/sd(obs,na.rm = T)
  }
  if(normalize =="mean_each"){
    rmse <- sqrt(mean(((mod-obs)/((mod+obs)/2))^2,na.rm = T))
  }
  return(rmse)
}


#' Title
#'
#' @param preds
#' @param actual
#'
#' @return
#' @export
#'
#' @examples
R2 <- function(preds, actual,adj=F,k=NULL) {
  NA_vals <- is.na(preds)|is.na(actual)
  preds <- preds[!NA_vals]
  actual <- actual[!NA_vals]
  rss <- sum((preds - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss / tss
  if(adj==T){
    if(is.null(k)){
      stop("for adjustet R2 k (number of independent regressors) needs to be defined")
    }
    n <- length(actual)
    rsq <- 1-((1-rsq)*(n-1)/(n-k-1))
  }
  return(rsq)
}
