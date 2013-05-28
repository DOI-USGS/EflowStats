#' Function to return the DH6 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual maximum daily flows for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh6 numeric containing the variability of the annual maximum daily flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh6(qfiletempf)
dh6 <- function(qfiletempf) {
  meandh6 <- dh1(qfiletempf, pref = "mean")
  maxbyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$wy_val), max, na.rm=TRUE)
  sddh6 <- sd(maxbyyear$x)
  dh6 <- (sddh6 * 100)/meandh6
  return(dh6)
}