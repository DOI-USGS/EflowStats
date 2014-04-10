#' Function to return the DH11 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH11; Annual maximum of 1-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 1-day moving average flow for each year. DH11 is the mean of these values divided by the median 
#' for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh11 numeric containing DH11 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh11(qfiletempf)
dh11 <- function(qfiletempf, pref = "mean") {
  meanmaxbyyear <- dh1(qfiletempf)
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  dh11 <- round(meanmaxbyyear/medianflow,digits=2)
  return(dh11)
}