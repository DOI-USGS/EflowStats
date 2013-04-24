#' Function to return the DH10 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual maximum 90-day moving average flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh10 numeric containing the variability of the annual maximum 90-day moving average flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh10(qfiletempf)
dh10 <- function(qfiletempf) {
  meandh10 <- dh5(qfiletempf, pref = "mean")
  day90mean <- rollmean(qfiletempf$discharge, 90, align = "right", 
                        na.pad = TRUE)
  day90rollingavg <- data.frame(qfiletempf, day90mean)
  rollingavgs90day <- subset(day90rollingavg, day90rollingavg$day90mean != 
                               "NA")
  max90daybyyear <- aggregate(rollingavgs90day$day90mean, 
                              list(rollingavgs90day$wy_val), max, na.rm=TRUE)
  sddh10 <- sd(max90daybyyear$x)
  dh10 <- (sddh10 * 100)/meandh10
  return(dh10)
}