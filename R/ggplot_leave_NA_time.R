#' Function to plot data with x-axis split into facets between longer periods without data
#'
#' @param y name of the column that should be ploted on y-axis as character
#' @param col name of the column that should be used for the colour as character
#' @param group name of the column that should be used for finding NA times as character the plot will automatically be split into different facets if the group column is NA for a defined timestep
#' @param data data that should be plotted
#' @param timestep timestep in minutes that is used to split plot into facets without longer gaps on the time axis
#' @param plot logical indicating whether output is a ggplot object (T) or a data.frame with the extra column \code{"period"} which can be used in facet_wrap to split time-axis
#' @param ylab label for y-axis
#' @param adj_grob_size logical whether grob size should be adjusted
#' @param ... other parameters that should be parsed to \code{scale_x_datetime(date_breaks = breaks, ...)}
#'
#' @return
#' @export
#'
#' @examples leave_NAtime_plot(data=data,group="CO2",plot=T)
leave_NAtime_plot <- function(y="CO2",
                              col="tiefe",
                              group="Pumpstufe",
                              data,
                              timestep = 10,#minutes
                              ylab="CO2 [ppm]",
                              plot=T,
                              adj_grob_size=T, ...,
                              geom="line"){
  
  data_order <- data[order(data$date),]
  data_sub <- data_order[!is.na(data_order[,group]),]
  starts<- data_sub$date[c(1,which(difftime(data_sub$date[-1],data_sub$date[-nrow(data_sub)],units = "mins") > timestep) +1)]
  ends<- data_sub$date[c(which(difftime(data_sub$date[-1],data_sub$date[-nrow(data_sub)],units = "mins") > timestep),nrow(data_sub))]
  
  period.df <- data.frame(starts,ends)
  
  data_sub$period <- NA
  for(i in 1:nrow(period.df)){
    data_sub$period[data_sub$date >= period.df$starts[i] & data_sub$date <= period.df$ends[i] ] <- i
  }
  
  col_lab <- T
  if(! col %in% colnames(data_sub)){
    col_lab <- F
    data_sub[,col] <- 1
  }
  if(plot ==F){
    return(data_sub)
  }else{
    if(geom == "line"){
      p <- ggplot(data_sub)+
        geom_line(aes(date,data_sub[,y],col=as.factor(data_sub[,col])))
    }else if(geom =="point"){
      p <- ggplot(data_sub)+
        geom_point(aes(date,data_sub[,y],col=as.factor(data_sub[,col])))
    }
    p<- p+
      facet_wrap(~period,scales="free_x",nrow=1)+
      theme(strip.text.x = element_blank())
    if(col_lab){
      p<- p+
        labs(col=col,y=ylab)
    }else{
      p<- p+
        guides(col=F)+
        labs(y=ylab)
    }
    if(adj_grob_size == T){
      p <- adj_grob_size(p,data_sub,...)
    }
    p
    #return(p)
  }
}

#' function to adjust the size of the facets used in \code{leave_NAtime_plot}
#'
#' @param p ggplot object
#' @param data data that is used for ggplot object
#' @param breaks character indicating breaks on x-axis eg. \code{"10  hours"} (default)
#' @param ... other parameters that should be parsed to \code{scale_x_datetime(date_breaks = breaks, ...)}
#'
#' @return
#' @export
#'
#' @examples adj_grob_size(plt2,plt_data,"1 day",date_labels= "%b %d")
adj_grob_size <- function(p, data ,breaks="10 hours",date_labels = "%b %d",plot=T, ...){
  # convert ggplot object to grob object
  p <- p+scale_x_datetime(date_breaks = breaks, date_labels = date_labels, ...)
  gp <- ggplotGrob(p)
  
  
  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]
  
  # get the number of unique x-axis values per facet (1 & 3, in this case)
  
  x.var <- sapply(unique(data$period),
                  function(x) difftime(max(data$date[data$period==x]),min(data$date[data$period==x]),units="mins"))
  #oder
  #x.var <- sapply(unique(data$period),
  #                function(x) length(which(data$period==x )))
  
  
  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$widths[facet.columns] <- gp$widths[facet.columns] * (x.var/sum(x.var))
  
  # plot result
  if(plot==T){
    grid::grid.draw(gp)
  }
  return(gp)
}
