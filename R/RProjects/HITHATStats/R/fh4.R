#' Function to return the FH4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flood pulse count (above 7 times the median) for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh4 numeric value of high flood pulse count for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh4(qfiletempf)
fh4 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 7 * ma2(qfiletempf)
  highflow <- subset(qfiletempf,qfiletempf$discharge>hfcrit)
  if (nrow(highflow)>0) {
  highbyyr <- aggregate(highflow$discharge,list(highflow$year_val),FUN=length)
  if (pref == "median") {
    fh4 <- median(highbyyr$x)
  }
  else {
    fh4 <- mean(highbyyr$x)
  }}
  else {
    fh4 <- 'NA'
  }
  return(fh4)
}