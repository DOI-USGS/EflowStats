#' Function to return the DH6 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH6; Variability of annual maximum daily flows. Compute the standard deviation for the maximum 1-day 
#' moving averages. DH6 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh6 numeric containing DH6 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh6(qfiletempf)
dh6 <- function(qfiletempf) {
  meandh6 <- dh1(qfiletempf, pref = "mean")
  maxbyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$wy_val), max, na.rm=TRUE)
  sddh6 <- sd(maxbyyear$x)
  dh6 <- round((sddh6 * 100)/meandh6,digits=2)
  return(dh6)
}