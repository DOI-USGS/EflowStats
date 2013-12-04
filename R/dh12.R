#' Function to return the DH12 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH12; Annual maximum of 7-day moving average flows divided by the median for the entire record. Compute the 
#' maximum daily average flow for each year. DH12 is the mean of these values divided by the median for the 
#' entire record (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh12 numeric containing DH12 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh12(qfiletempf)
dh12 <- function(qfiletempf) {
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  meanmax7day <- dh3(qfiletempf)
  dh12 <- meanmax7day/medianflow
  return(dh12)
}