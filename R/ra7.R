#' Function to return the RA7 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA7; Change of flow. Compute the loge of the flows for the entire flow record. Compute the change in log of 
#' flow for days in which the change is negative for the entire flow record. RA7 is the median of these log 
#' values (cubic feet per second/day-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra7 numeric containing RA7 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ra7(qfiletempf)
ra7 <- function(qfiletempf) {
  disch_log <- ifelse(qfiletempf$discharge>0,log(qfiletempf$discharge),log(.01))
  diffbtdays <- diff(disch_log, lag = 1, 
                     differences = 1)
  findfallvalues <- subset(diffbtdays, diffbtdays < 
                             0)
  ra7 <- median(abs(findfallvalues))
  return(ra7)
}