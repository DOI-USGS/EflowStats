#' Function to return the coefficient of variation for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the coefficient of variation
#' 
#' @param x data frame containing value data for the chosen timeseries
#' @return cv coefficient of variation for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' cv(qfiletempf$discharge)
cv <- function(x) {
  x1 <- mean(x,na.rm=TRUE)
  x2 <- sdev(x)
  cv <- x2/x1
  return(cv)
}