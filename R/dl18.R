#' Function to return the DL18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL18; Number of zero-flow days. Count the number of zero-flow days for the entire flow record. DL18 is the 
#' mean (or median-Use Preference option) annual number of zero flow days (number of days/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl18 numeric containing DL18 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl18(qfiletempf)
dl18 <- function(qfiletempf, pref = "mean") {
  subset_zero <- subset(qfiletempf,qfiletempf$discharge==0)
  if (nrow(subset_zero)>0) {
  subset_zero$discharge <- subset_zero$discharge+1
  zero_cnts <- aggregate(subset_zero$discharge, list(qfiletempf$wy_val), sum)
  } else {zero_cnts <- data.frame(x=rep(0,1))}
  if (pref == "median") {
    dl18 <- median(zero_cnts$x)
  }
  else {
    dl18 <- mean(zero_cnts$x)
  }
  return(dl18)
}