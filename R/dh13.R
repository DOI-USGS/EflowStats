#' Function to return the DH13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH13; Annual maximum of 30-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 30-day moving average flow for each year. DH13 is the mean of these values divided by the median 
#' for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh13 numeric containing DH13 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh13(qfiletempf)
dh13 <- function(qfiletempf) {
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  meanmax30day <- dh4(qfiletempf)
  dh13 <- round(meanmax30day/medianflow,digits=2)
  return(dh13)
}