#' Function to return the DH11 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean annual maximum daily flow divided by the median flow of the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh11 numeric containing the mean annual maximum daily flow divided by the median flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh11(qfiletempf)
dh11 <- function(qfiletempf, pref = "mean") {
  meanmaxbyyear <- dh1(qfiletempf)
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  dh11 <- meanmaxbyyear/medianflow
  return(dh11)
}