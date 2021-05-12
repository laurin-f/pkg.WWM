#############################
#funktion definieren um wechsel zu identifizieren
#' Function to identify periods in sensor calibration
#'
#' @param data measured data
#' @param col which column should be used
#' @param ppm_lim limit for ppm change in same period
#' @param min_lim maximal time difference of data points in one period
#' @param puffer_start amount of minutes to cut off at the begin
#' @param puffer_ende amount of minutes to cut off at the end
#'
#' @return
#' @export
#'
#' @examples sampler2_list <- wechsel_fun(data=data_sampler2,col="CO2_Dyn_02")
wechsel_fun <- function(data,
                        col="CO2",
                        ppm_lim = 60,
                        min_lim = 10,
                        puffer_start=10,
                        puffer_ende=1){
  wechsel<-
    which(abs(diff(data[,col])) > ppm_lim | as.numeric(difftime(data$date[-1],data$date[-nrow(data)],units = "mins")) > min_lim)
  start<-c(1,wechsel+1)
  ende<-c(wechsel,nrow(data))

  #start und endpunkte zusammenfügen, punkte bei denen start und endpunkt zu Nah sind kommen weg

  intervalls_min <- c(NA,difftime(data$date[-1],data$date[-nrow(data)],unit="secs"))

  start_puffer <- sapply(start,function(x) which.min(abs(data$date - (data$date[x] + puffer_start*60))))
  ende_puffer <- sapply(ende,function(x) which.min(abs(data$date - (data$date[x] - puffer_ende*60))))

  data$date[start]-data$date[start_puffer]

  periode <- cbind(start_puffer,ende_puffer)[ende_puffer - start_puffer > 0,]

  wechsel_dates <- data.frame(start = data$date[periode[,1]],
                              ende =  data$date[periode[,2]])

  #spalte kal_punkt an data anfügen und durchnummerieren
  data$kal_punkt<-NA
  for(i in 1:nrow(periode)){
    periode.i <- periode[i,1]:periode[i,2]
    data$kal_punkt[periode.i]<-i
  }

  #testplot ob die einteilung geklappt hat
  leave_NAtime_plot(y=col,data=data,group=col,col="kal_punkt",geom="point",breaks="1 hour",date_labels= "%H:%M")

  return(list(data,wechsel_dates))
}
