#' Function to return the DL6 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual minimum daily average flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl6 numeric containing the variability of the annual minimum daily average flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl6(qfiletempf)
dl6 <- function(qfiletempf) {
  meandl6 <- dl1(qfiletempf, pref = "mean")
  annualminimum <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), min)
  sddl6 <- sd(annualminimum$x)
  dl6 <- (sddl6 * 100)/meandl6
  return(dl6)
}