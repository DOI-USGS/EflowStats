#' Function to return the DL13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL13; Annual minimum of 30-day moving average flow divided by the median for the entire record. Compute the 
#' minimum of a 30-day moving average flow for each year. DL13 is the mean of these values divided by the median 
#' for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl13 numeric containing DL13 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl13(qfiletempf)
dl13 <- function(qfiletempf) {
  meanmin <- dl4(qfiletempf)
  medianq <- median(qfiletempf$discharge)
  dl13 <- round(meanmin/medianq,digits=2)
  return(dl13)
}