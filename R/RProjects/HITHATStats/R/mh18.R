#' Function to return the MH18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates MH18. Variability across annual maximum flows. Compute the log10s of the maximum annual 
#' flows. Find the standard deviation and mean for these values. MH18 is the sd times 100 divided by the mean.
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh18 numeric value of the standard deviation times 100 divided by the mean for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' mh18(x)
mh18 <- function(x) {
  maxbyyr <- aggregate(x$discharge,list(x$year_val),FUN=max,na.rm=TRUE)
  colnames(maxbyyr) <- c("Year","yrmax")
  log10maxbyyr <- log10(maxbyyr$yrmax)
  mh18 <- (sd(log10maxbyyr)*100)/mean(log10maxbyyr)
  return(mh18)
}