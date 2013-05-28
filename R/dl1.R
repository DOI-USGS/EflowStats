#' Function to return the DL1 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of the annual minimum daily flows for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl1 numeric containing the mean of the annual minimum daily flows for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl1(qfiletempf)
dl1 <- function(qfiletempf, pref = "mean") {
  annualminimum <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), min)
  if (pref == "median") {
    dl1 <- median(annualminimum$x)
  }
  else {
    dl1 <- mean(annualminimum$x)
  }
  return(dl1)
}