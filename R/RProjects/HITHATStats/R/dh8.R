#' Function to return the DH8 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual maximum 7-day moving average flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh8 numeric containing the variability of the annual maximum 7-day moving average flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh8(qfiletempf)
dh8 <- function(qfiletempf) {
  meandh8 <- dh3(qfiletempf, pref = "mean")
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                        na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                               "NA")
  max7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$year_val), max, na.rm=TRUE)
  sddh8 <- sd(max7daybyyear$x)
  dh8 <- (sddh8 * 100)/meandh8
  return(dh8)
}