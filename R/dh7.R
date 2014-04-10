#' Function to return the DH7 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH7; Variability of annual maximum of 3-day moving average flows. Compute the standard deviation for the 
#' maximum 3-day moving averages. DH7 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh7 numeric containing DH7 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh7(qfiletempf)
dh7 <- function(qfiletempf) {
  meandh7 <- dh2(qfiletempf, pref = "mean")
  day3mean <- rollmean(qfiletempf$discharge, 3, align = "right", 
                       na.pad = TRUE)
  day3rollingavg <- data.frame(qfiletempf, day3mean)
  rollingavgs3day <- subset(day3rollingavg, day3rollingavg$day3mean != 
                              "NA")
  max3daybyyear <- aggregate(rollingavgs3day$day3mean, 
                             list(rollingavgs3day$wy_val), max, na.rm=TRUE)
  sddh7 <- sd(max3daybyyear$x)
  dh7 <- round((sddh7 * 100)/meandh7,digits=2)
  return(dh7)
}