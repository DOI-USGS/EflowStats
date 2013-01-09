#' Function to return the MH20 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates NH20. Specific mean annual maximum flow. MH20 is the mean (or median - use preference option) 
#' of the annual maximum flows divided by the drainage area.
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @param drainarea numeric value of drainage area for a site
#' @param pref string containing a "mean" or "median" preference
#' @return mh20 numeric value of the mean or median of annual maximum flows divided by the drainage area for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' drainarea<-56.5
#' mh20(x,drainarea)
mh20 <- function(x,drainarea,pref = "mean") {
  maxbyyr <- aggregate(x$discharge,list(x$year_val),FUN=max,na.rm=TRUE)
  colnames(maxbyyr) <- c("Year","yrmax")
  if (pref == "median") {
    mh20 <- (median(maxbyyr$yrmax))/drainarea
  } 
  else {
    mh20 <- (mean(maxbyyr$yrmax))/drainarea
  }
  return(mh20)
}