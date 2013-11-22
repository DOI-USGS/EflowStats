#' Function to return the FH4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH4; High flood pulse count. Compute the average number of days per year that the flow is above a threshold 
#' equal to seven times the median flow for the entire record. FH4 is the mean (or median - Use Preference option) 
#' of the annual number of days for all years (number of days/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh4 numeric value of high flood pulse count for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh4(qfiletempf)
fh4 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 7 * ma2(qfiletempf)
  highflow <- subset(qfiletempf,qfiletempf$discharge>hfcrit)
  if (nrow(highflow)>0) {
  highbyyr <- aggregate(highflow$discharge,list(highflow$wy_val),FUN=length)
  numrows <- nrow(highbyyr)
  numyrs <- length(unique(qfiletempf$wy_val))
  if (numrows<numyrs) { highbyyr_x <- c(highbyyr$x,rep(0,numyrs-numrows)) } else { highbyyr_x <- highbyyr$x}
  if (pref == "median") {
    fh4 <- median(highbyyr_x)
  }
  else {
    fh4 <- mean(highbyyr_x)
  }}
  else {
    fh4 <- 0
  }
  return(fh4)
}