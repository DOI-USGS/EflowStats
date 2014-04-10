#' Function to return the MH20 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates MH20, specific mean annual maximum flow. MH20 is the mean (or median-Use Preference option) of 
#' the annual maximum flows divided by the drainage area (cubic feet per second/square mile-temporal).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @param drainarea numeric value of drainage area for a site
#' @param pref string containing a "mean" or "median" preference
#' @return mh20 numeric value of MH20 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' drainarea<-56.5
#' mh20(qfiletempf,drainarea)
mh20 <- function(x,drainarea,pref = "mean") {
  maxbyyr <- aggregate(x$discharge,list(x$wy_val),FUN=max,na.rm=TRUE)
  colnames(maxbyyr) <- c("Year","yrmax")
  if (pref == "median") {
    mh20 <- round((median(maxbyyr$yrmax))/drainarea,digits=2)
  } 
  else {
    mh20 <- round((mean(maxbyyr$yrmax))/drainarea,digits=2)
  }
  return(mh20)
}