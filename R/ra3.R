#' Function to return the RA3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA3; Fall rate. Compute the change in flow for days in which the change is negative for the entire flow record. 
#' RA3 is the mean (or median-Use Preference option) of these values (cubic feet per second/day-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ra3 numeric containing RA3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra3(qfiletempf)
ra3 <- function(qfiletempf, pref = "mean") {
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findfallvalueneg <- subset(diffbtdays, diffbtdays < 
                               0)
  findfallvalues <- abs(findfallvalueneg)
  if (pref == "median") {
    ra3 <- round(median(findfallvalues),digits=2)
  }
  else {
    ra3 <- round(mean(findfallvalues),digits=2)
  }
  return(ra3)
}