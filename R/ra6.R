#' Function to return the RA6 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA6; Change of flow. Compute the loge of the flows for the entire flow record. Compute the change in log of flow 
#' for days in which the change is positive for the entire flow record. RA6 is the median of these values (cubic feet 
#' per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra6 numeric containing RA6 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra6(qfiletempf)
ra6 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  disch_log <- ifelse(qfiletempf$discharge>0,log(qfiletempf$discharge),log(.01))
  diffbtdays <- diff(disch_log, lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays > 
                             0)
  ra6 <- round(median(abs(findrisevalues)),digits=2)
  return(ra6)
}