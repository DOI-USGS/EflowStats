#' Function to return the DH1 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH1; Annual maximum daily flow. Compute the maximum of a 1-day moving average flow for each year. DH1 is the 
#' mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh1 numeric containing DH1 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh1(qfiletempf)
dh1 <- function(qfiletempf, pref = "mean") {
  annualmax <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), max)
  if (pref == "median") {
    dh1 <- median(annualmax$x)
  }
  else {
    dh1 <- mean(annualmax$x)
  }
  return(dh1)
}