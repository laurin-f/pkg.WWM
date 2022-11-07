
#' @title load packages and install them if they are not already installed
#'
#' @param pkgs character string with package names
#'
#' @return nothing returned
#' @export
#'
#' @examples
#' packages<-c("stringr","lubridate","RSQLite","odbc","reshape2","zoo","ggplot2","ggnewscale","xlsx","readxl")
#'
#' check.packages(packages)
check.packages<-function(pkgs){
  old.pkgs<-installed.packages()
  new.pkgs<-pkgs[!pkgs %in% old.pkgs[,"Package"]]
  if(length(new.pkgs)>0){
    install.packages(new.pkgs, dependencies = TRUE)
  }
  sapply(pkgs, require,character.only=T)
}


# packages<-c("stringr","lubridate","dplyr","pkg.WWM","data.table","ggplot2","fastglm")
# check.packages(packages)
