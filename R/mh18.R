#' Function to return the MH18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates MH18, variability across annual maximum flows. Compute the logs (log10) of the maximum annual flows. 
#' Find the standard deviation and mean for these values. MH18 is the standard deviation times 100 divided by the 
#' mean (percent-spatial).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh18 numeric value of mh18 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh18(qfiletempf)
mh18 <- function(x) {
  maxbyyr <- aggregate(x$discharge,list(x$wy_val),FUN=max,na.rm=TRUE)
  colnames(maxbyyr) <- c("Year","yrmax")
  log10maxbyyr <- log10(maxbyyr$yrmax)
  mh18 <- round((sd(log10maxbyyr)*100)/mean(log10maxbyyr),digits=2)
  return(mh18)
}