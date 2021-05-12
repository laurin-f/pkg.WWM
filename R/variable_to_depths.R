#' Funktion um messungen einer Variablen im wide Format für andere Messtiefen zu interpolieren
#'
#' @param variable name variablen es müssen die spalten im wide format vorliegen mit spaltennamen: "variable_tiefe" z.B. "VWC_-5"
#' @param colname name der spalte mit den interpolierten Werten der Variablen
#' @param df dataframe
#'
#' @return
#' @export
#'
#' @examples data <- variable_to_depths("VWC")
#' data <- variable_to_depths("T_C","T_soil")
variable_to_depths <- function(variable,colname=NULL,df = data){
  if(is.null(colname)){
    colname <- variable
  }
  #tiefe aus den variable spalten ausschneidenB
  col_tiefe <- -abs(as.numeric(str_extract(colnames(df),paste0("(?<=",variable,"_)\\d+"))))
  df[,colname] <- NA
  #schleife um variable Daten für die Messtiefen zu Interpolieren
  for(i in unique(df$tiefe)){
    #subset für tiefe == i
    df_i <- subset(df,tiefe == i)
    #differenz der variable messtiefen und der CO2 messtiefen
    diff_pos <- col_tiefe - i
    diff_neg <- diff_pos

    #bei einem Vektor nur die Positiven und bei einem nur die negativen differenzen übrig lassen
    sign <- diff_pos > 0
    diff_pos[!sign] <- NA
    diff_neg[sign] <- NA

    #index der niedrigsten positiven und negativen differenz der Messtiefen
    col_pos <- order(diff_pos)[1]
    col_neg <- order(-diff_neg)[1]

    #wenn eine positive Differenz dabei ist wird für variable das gewichtete mittel zwischen der nächst höheren und niedrigeren Messtiefe bestimmt
    if(any(sign,na.rm = T)){
      #die Gewichtung entspricht dem Abstand zwischen der CO2 messtiefe und der variable messtiefe
      weights <- c(min(diff_pos,na.rm = T),min(-diff_neg,na.rm = T))
      variable_i <- (df_i[,col_pos]*weights[2]+df_i[,col_neg]*weights[1])/sum(weights)

      #wenn keine positive Differenz dabei ist (also keine höhere variable Messtiefe vorliegt) wird für variable der Wert der nächst tieferen Messtiefe übernommen
    }else{
      variable_i <- df_i[,col_neg]
    }
    df[df$tiefe == i,colname] <- variable_i
  }#ende
  return(df)
}#ende function
