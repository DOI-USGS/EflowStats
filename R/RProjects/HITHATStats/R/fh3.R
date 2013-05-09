#' Function to return the FH3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flood pulse count (above 3 times the median) for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh3 numeric value of high flood pulse count for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh3(qfiletempf)
fh3 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 3 * ma2(qfiletempf)
  highflow <- subset(qfiletempf,qfiletempf$discharge>hfcrit)
  if (nrow(highflow)>0) {
  highbyyr <- aggregate(highflow$discharge,list(highflow$wy_val),FUN=length)
  numrows <- nrow(highbyyr)
  numyrs <- length(unique(qfiletempf$wy_val))
  if (numrows<numyrs) { highbyyr_x <- c(highbyyr$x,rep(0,numyrs-numrows)) } else { highbyyr_x <- highbyyr$x}
  if (pref == "median") {
    fh3 <- median(highbyyr_x)
  }
  else {
    fh3 <- mean(highbyyr_x)
  }}
  else {
    fh3 <- 'NA'
  }
  return(fh3)
}