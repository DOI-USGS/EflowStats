#' Internal function to return the MA2 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the median of the daily flow values for the entire record (cubic feet per second)
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ma2 numeric value of the median daily flow for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma2(qfiletempf)
ma2 <- function(x) {
  ma2 <- round(median(x$discharge,na.rm=TRUE),digits=2)
  return(ma2)
}