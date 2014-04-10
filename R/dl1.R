#' Function to return the DL1 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL1; Annual minimum daily flow. Compute the minimum 1-day average flow for each year. DL1 is the mean 
#' (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl1 numeric containing DL1 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl1(qfiletempf)
dl1 <- function(qfiletempf, pref = "mean") {
  annualminimum <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), min)
  if (pref == "median") {
    dl1 <- round(median(annualminimum$x),digits=2)
  }
  else {
    dl1 <- round(mean(annualminimum$x),digits=2)
  }
  return(dl1)
}