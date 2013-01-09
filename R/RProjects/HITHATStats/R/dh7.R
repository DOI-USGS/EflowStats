#' Function to return the DH7 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual maximum 3-day moving average flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh7 numeric containing the variability of the annual maximum 3-day moving average flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh7(qfiletempf)
dh7 <- function(qfiletempf) {
  meandh7 <- dh2(qfiletempf, pref = "mean")
  day3mean <- rollmean(qfiletempf$discharge, 3, align = "right", 
                       na.pad = TRUE)
  day3rollingavg <- data.frame(qfiletempf, day3mean)
  rollingavgs3day <- subset(day3rollingavg, day3rollingavg$day3mean != 
                              "NA")
  max3daybyyear <- aggregate(rollingavgs3day$day3mean, 
                             list(rollingavgs3day$year_val), max, na.rm=TRUE)
  sddh7 <- sd(max3daybyyear$x)
  dh7 <- (sddh7 * 100)/meandh7
  return(dh7)
}