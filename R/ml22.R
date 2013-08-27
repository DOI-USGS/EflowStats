#' Function to return the ML22 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and a numeric drainage area and  
#' calculates ML22, specific mean annual minimum flow.  ML22 is the mean (or median-Use Preference option) of the 
#' annual minimum flows divided by the drainage area (cubic feet per second/square mile-temporal).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @param drainarea numeric value of the drainage area for a given site
#' @return ml22 numeric value of ML22 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' drainarea<-56.5
#' ml22(x,drainarea)
ml22 <- function(x,drainarea,pref = "mean") {
  minbyyr <- aggregate(x$discharge,list(x$wy_val),FUN=min,na.rm=TRUE)
  colnames(minbyyr) <- c("Year","yrmin")
  if (pref == "median") {
    ml22 <- (median(minbyyr$yrmin))/drainarea
  } 
  else {
    ml22 <- (mean(minbyyr$yrmin))/drainarea
  }
  return(ml22)
}