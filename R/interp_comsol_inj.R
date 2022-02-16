#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
interp_comsol_inj <- function(...){
  out_list <- list()
  for(i in c("min","max")){
    print(paste("inj_rate = ",i))
  out_list[[i]] <- run_comsol_nruns(inj_fun = i, file_suffix = i, ...)
  }
  approx_df <- out_list[[1]][,c("date","tiefe")]
  
  
  for(i in 1:nrow(approx_df)){
    approx_xy <- approx(x=c(out_list[["min"]][i,"mod_inj_rate"],out_list[["max"]][i,"mod_inj_rate"]),
                        y=c(out_list[["min"]][i,"DSD0"],out_list[["max"]][i,"DSD0"]),
                        xout = as.numeric(data$inj_mol_m2_s[data$date == out_list[["min"]][i,"date"]][1])
    )
    approx_df$DSD0[i] <- approx_xy$y
    approx_df$inj_mol_m2_s[i] <- approx_xy$x
  }
  
  return(approx_df)
}
