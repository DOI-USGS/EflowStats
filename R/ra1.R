#' Function to return the RA1 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA1; Rise rate. Compute the change in flow for days in which the change is positive for the entire flow record. 
#' RA1 is the mean (or median-Use Preference option) of these values (cubic feet per second/day-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ra1 numeric containing RA1 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra1(qfiletempf)
ra1 <- function(qfiletempf, pref = "mean") {
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays > 
                             0)
  if (pref == "median") {
    ra1 <- round(median(findrisevalues),digits=2)
  }
  else {
    ra1 <- round(mean(findrisevalues),digits=2)
  }
  return(ra1)
}