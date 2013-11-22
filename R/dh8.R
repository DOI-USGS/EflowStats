#' Function to return the DH8 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH8; Variability of annual maximum of 7-day moving average flows. Compute the standard deviation for the 
#' maximum 7-day moving averages. DH8 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh8 numeric containing DH8 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh8(qfiletempf)
dh8 <- function(qfiletempf) {
  meandh8 <- dh3(qfiletempf, pref = "mean")
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                        na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                               "NA")
  max7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$wy_val), max, na.rm=TRUE)
  sddh8 <- sd(max7daybyyear$x)
  dh8 <- (sddh8 * 100)/meandh8
  return(dh8)
}