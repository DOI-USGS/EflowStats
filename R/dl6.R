#' Function to return the DL6 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL6; Variability of annual minimum daily average flow. Compute the standard deviation for the minimum daily 
#' average flow. DL6 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl6 numeric containing DL6 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl6(qfiletempf)
dl6 <- function(qfiletempf) {
  meandl6 <- dl1(qfiletempf, pref = "mean")
  annualminimum <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), min)
  sddl6 <- sd(annualminimum$x)
  dl6 <- round((sddl6 * 100)/meandl6,digits=2)
  return(dl6)
}