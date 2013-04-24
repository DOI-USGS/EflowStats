#' Function to return the DL18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of the annual number of zero-flow days for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl18 numeric containing the mean of the annual number of zero-flow days for the given data frame
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