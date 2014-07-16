#' Function to return the skewness for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the skewness
#' 
#' @param x data frame containing value data for the chosen timeseries
#' @return skew skewness for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' skew(qfiletempf)
skew <- function(x) {
  x1 <- mean(x$discharge,na.rm=TRUE)
  x2 <- median(x$discharge,na.rm=TRUE)
  x3 <- sd(x$discharge,na.rm=TRUE)
  skew <- (x1-x2)/x3
  return(skew)
}