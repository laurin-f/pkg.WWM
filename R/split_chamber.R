#' Function to automatically identify closing and opening times of chamber measurents
#'
#'
#' @param data data.frame with gas concentrations of several chamber measurements
#' @param closing_lim maximal concentration-gradient before closing the chamber
#' @param opening_lim minimal concentration-gradient before opening the chamber
#' @param t_max maximal measurement time (minutes), if time between closing and opening
#' exceeds this time the rest of the measurement will not be included for the calculation
#' of the flux
#' @param t_init initial time (minutes) after closing of the chamber that will be excluded from the calculations
#' @param t_min minimum timespan for each chamber measurement in minutes
#' @param gas name of the gas as character
#' @param adj_openings logical; if \code{TRUE} opening times will be adjusted
#' to never be smaller than closing times
#' @param round_intervall
#'
#' @return input data.frame with colums messid (number of the measurement) and zeit (time in minutes after closing) added
#' @export
#' @import lubridate
#'
#' @examples
#' test <- split.chamber(data,
#' closing_th = 40,
#' opening_th = -40,
#' t_max=Inf,
#' t_init=0,
#' t_min=5)
split_chamber <- function(data,
                          closing_lim = 100,
                          opening_lim = -100,
                          t_lim = 1,#minute
                          t_max = 3,#minute
                          t_init = 0.1,#minute
                          t_min = 4,#minute
                          gas = "CO2",
                          adj_openings = T) {
  ##############################
  #datensatz aggregieren
  
  #remove na rows of gas and date
  data <- data[!is.na(data[, gas]) & !is.na(data$date), ]
  
  #data nach datum sortieren und hourminute aus date ausschneiden
  data <- data[order(data$date), ]
  data$hourminute <- paste0(format(data$date, "%Y-%m-%d %H:%M"), ":00")
  #duplicate von hourminute entfernen
  #sodass immer nur der erste Werte pro minute bleibt
  data$hourminute[duplicated(data$hourminute)] <- NA
  
  #spalte mit minutenwerten
  #data$rowid <- 1:nrow(data) 
  #nach minutenwerten aggregieren
  agg_cols <- which(sapply(data,is.numeric))
  data.agg <-
    aggregate(data[,agg_cols], list(hourminute = data$hourminute), mean)
  #date formatieren
  data.agg$date <- ymd_hms(data.agg$hourminute)
  #data.agg <- data.agg[data.agg$hourminute %in% data$hourminute,]
  
  #########################################################
  #am aggregierten Datensatz kammermessungen identifizieren
  
  #differenz der Gaswerte before und after sind identisch nur um eins verschoben
  before <- c(NA, diff(data.agg[, gas]))
  after <- c(diff(data.agg[, gas]), NA)
  #Zeitdifferenz in Minuten before und after sind identisch nur um eins verschoben
  timediff_before <- c(NA, as.numeric(diff(data.agg$date)))
  timediff_after <- c(as.numeric(diff(data.agg$date)), NA)
  
  #Punkte an denen die Schwellenwerte für closing bzw. opening vorliegen
  closing <- which((before  < closing_lim &
                      after  > closing_lim)|
                     (timediff_before > t_lim))
  
  opening <- which((before  > opening_lim &
                      after < opening_lim)|
                     (timediff_after > t_lim))
  
  
  
  ###################################################################
  #adjust openings
  #adj openings bedeutet opening wird so umgeschrieben das immer closing und opening im Wechsel vorkommen
  if (length(opening) > 0 & length(closing) > 0) {
    if (adj_openings == T) {
      #wenn der letzte Wert von Opening kleiner ist als bei closing wird bei Opening nrow hinzugefügt
      if (max(opening) < max(closing)) {
        opening <- c(opening, nrow(data.agg))
      }
      #solange der erste wert bei opening kleiner ist als bei closing
      #wird bei closing 1 als erster Wert hinzugefügt
      if (opening[1] < closing[1]) {
        closing <- c(1,closing)
      }
      if (length(closing) > 1) {
        #alle weiteren Werte von closing werden iterativ getestet
        for (i in 2:length(closing)) {
          if(i > length(closing)){
            break
          }
          # #wenn opening[i-1] na ist wird an dieser stelle closing[i] -1 eingesetzt
          # if (is.na(opening[i - 1])) {
          #   opening[i - 1] <- closing[i] - 1
          # }
          #wenn opening[i-1] größer ist als closing[i] wird closing[i] entfernt solange bis das nicht mehr der fall ist
          while (opening[i - 1] > closing[i]) {
            closing <- closing[-i]
            if(i > length(closing)){
              break
            }
            #opening <-
            #  c(opening[0:(i - 2)], closing[i] - 1, opening[(i - 1):length(opening)])
          }
          if(i > length(closing)){
            break
          }
          
          #solange opening[i] kleiner gleich closing[i] ist wird opening[i] entfernt
          #wenn kein opening[i] mehr da ist wird nrow(data.agg) eingefügt
          while (opening[i] <= closing[i]) {
            opening <- opening[-i]
            # if (is.na(opening[i])) {
            #   opening[i] <- nrow(data.agg)
            # }#ende if
          }#ende while
        }#ende for
        if (length(opening) > length(closing)) {
          opening <- opening[-length(opening)]
        }
      }#ende if
      
    }#ende adj_openings
    
    #differenz der längen opening und closing
    if(length(opening) != length(closing)){
      data$messid <- NA
      data$zeit <- NA
      if(adj_openings == F){
        warning("length(opening) != length(closing) \ntry adj_openings = T")
      }else{
        warning("length(opening) != length(closing) \nchange input parameters")
      }
    }else if(!all(closing < opening)){
      data$messid <- NA
      data$zeit <- NA
      if(adj_openings == F){
        warning("!all(closing < opening) \ntry adj_openings = T")
      }else{
        warning("!all(closing < opening) \nchange input parameters")
      }
    }else{
      #nur die closing opening perioden die mindestens
      #t_min minutenwerte enthalten wählen
      diff_open_close <- (opening - closing) >= t_min
      opening <- opening[diff_open_close]
      closing <- closing[diff_open_close]
      
      #################################################################
      #Kammermesszeiträume vom aggregierten auf nicht aggregierten
      #Datensatz übertragen
      
      #zeitpunkte von closing und opening als character
      closing.time <- data.agg$hourminute[closing]
      opening.time <- data.agg$hourminute[opening]
      
      
      
      #index von closing und opening des nicht aggregierten data.frames
      closingID <- which(data$hourminute %in% closing.time)
      openingID <- which(data$hourminute %in% opening.time)
      if (length(openingID) < length(closingID)) {
        openingID[length(closingID)] <- nrow(data)
      }
      
      #zeit und messid an data anfügen
      data$zeit <- NA
      data$messid <- NA
      for (i in 1:length(openingID)) {
        #zeit in minuten nach closing
        data$zeit[closingID[i]:openingID[i]] <-
          difftime(data$date[closingID[i]:openingID[i]], data$date[closingID[i]], unit =
                     "mins")
        #messid als durchlaufende Nummer für jede closing opening periode
        data$messid[closingID[i]:openingID[i]] <- i
      }
      
      #zeiträume zuschneiden um nur werte zwischen t_init und t_max zu haben
      data$zeit[data$zeit > t_max | data$zeit < t_init] <- NA
      #diese Zeiträume auch bei messid mit NA überschreiben
      data$messid[is.na(data$zeit)] <- NA
      
    }
    ##################################################
    #plot um ergebnis zu teste
    
    #spalte mit opening und closing punkten
    data.agg$change <- ""
    data.agg$change[opening] <- "opening"
    data.agg$change[closing] <- "closing"
    
    #messidspalte
    data.agg$messid <- NA
    data.agg$messid[opening] <- seq_along(opening)
    data.agg$messid[closing] <- seq_along(closing)
    
    #Farben für plot
    #kein ggplot da funktion dann schneller ist
    if(!all(is.na(data$messid))){
      messid_cols <-
        scales::hue_pal()(max(data$messid, na.rm = T))[data$messid]
    }else{
      messid_cols <- NA
    }
    #plot
    par(mfrow = c(2, 1), mar = c(1, 3, 1, 1))
    plot(
      data.agg$date,
      data.agg[, gas],
      col = ifelse(data.agg$change == "", 1, NA),
      pch = 20,
      xlab = ""
    )
    points(data$date, data[, gas], col = messid_cols)
    points(
      data.agg$date,
      data.agg[, gas],
      col = ifelse(
        data.agg$change == "",
        NA,
        ifelse(data.agg$change == "opening", 2, 3)
      ),
      pch = as.character(data.agg$messid)
    )
    title(main=paste(range(data$date,na.rm = T),collapse = " to "))
    
    legend(
      "topleft",
      c("opening", "closing", unique(data$messid)),
      col = c(2:3, unique(messid_cols)),
      pch = 20,
      bty = "n"
    )
    
    before_afters <-
      c(closing_lim,
        opening_lim)
    plot(data.agg$date, before, xlab = "", ylim = c(min(before_afters) - 10, 2 * max(before_afters)),pch=20)
    abline(h = closing_lim, col = 3)
    abline(h = opening_lim, col = 2)
    abline(v = data.agg$date[closing], col = 3)
    abline(v = data.agg$date[opening], col = 2)
    points(data.agg$date,after, pch = 20, col = 4)
    pairs <- c(t(cbind(after,before,NA)))
    increase <- rep(after > before,each = 3)
    lines(rep(data.agg$date,each = 3)[increase],pairs[increase],col=4)
    lines(rep(data.agg$date,each = 3)[!increase],pairs[!increase],col="black")
    
    
    legend(
      "bottomleft",
      c("before", "after", "closing", "opening"),
      col = c(1, 4, 3, 2),
      pch = c(20, 20, NA, NA),
      lty = c(NA, NA, 1, 1),
      bty = "n"
    )
    par(mfrow = c(1, 1))
    
  }else{
    #ende if length openingclosing > 1
    par(mfrow = c(2, 1), mar = c(1, 3, 1, 1))
    plot(data.agg[, gas], pch = 20, xlab = "")
    
    before_afters <-
      c(closing_lim,
        opening_lim)
    plot(data.agg$date, before, xlab = "", ylim = c(min(before_afters) - 10, 2 * max(before_afters)))
    abline(h = closing_lim, col = 3)
    abline(h = opening_lim, col = 2)
    points(data.agg$date,after, pch = 3, col = 4)
    pairs <- c(t(cbind(after,before,NA)))
    increase <- rep(after > before,each = 3)
    lines(rep(data.agg$date,each = 3)[increase],pairs[increase],col=4)
    lines(rep(data.agg$date,each = 3)[!increase],pairs[!increase],col="black")
    
    
    legend(
      "bottomleft",
      c("before", "after", "closing", "opening"),
      col = c(1, 4, 3, 2),
      pch = c(20, 20, NA, NA),
      lty = c(NA, NA, 1, 1),
      bty = "n"
    )
    par(mfrow = c(1, 1))
    stop("no openings and closings found")
    
  }
  return(data)
}
