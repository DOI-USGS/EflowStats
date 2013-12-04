#' Function to return the ML21 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates ML21, variability across annual minimum flows. Compute the mean and standard deviation for the 
#' annual minimum flows. ML21 is the standard deviation times 100 divided by the mean (percent-spatial). 
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml21 numeric value of ML21 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml21(qfiletempf)
ml21 <- function(x) {
  minbyyr <- aggregate(x$discharge,list(x$wy_val),FUN=min,na.rm=TRUE)
  colnames(minbyyr) <- c("Year","yrmin")
  ml21 <- (sd(minbyyr$yrmin)*100)/mean(minbyyr$yrmin)
  return(ml21)
}