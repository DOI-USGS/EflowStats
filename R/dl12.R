#' Function to return the DL12 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL12; Annual minimum of 7-day moving average flow divided by the median for the entire record. Compute the 
#' minimum of a 7-day moving average flow for each year. DL12 is the mean of these values divided by the median 
#' for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl12 numeric containing DL12 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl12(qfiletempf)
dl12 <- function(qfiletempf) {
  meanmin <- dl3(qfiletempf)
  medianq <- median(qfiletempf$discharge)
  dl12 <- meanmin/medianq
  return(dl12)
}