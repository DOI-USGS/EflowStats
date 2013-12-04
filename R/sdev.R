#' Function to return the standard deviation for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the standard deviation
#' 
#' @param x data frame containing value data for the chosen timeseries
#' @return sdev standard deviation for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' sdev(qfiletempf)
sdev <- function(x) {
  sdev <- sd(x$discharge,na.rm=TRUE)
  return(sdev)
}