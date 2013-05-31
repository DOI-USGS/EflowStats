#' Function to return the DL8 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL8; Variability of annual minimum of 7-day moving average flow. Compute the standard deviation for the 
#' minimum 7-day moving averages. DL8 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl8 numeric containing DL8 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl8(qfiletempf)
dl8 <- function(qfiletempf) {
  meandl8 <- dl3(qfiletempf, pref = "mean")
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                              "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$wy_val), min, na.rm=TRUE)
  sddl8 <- sd(min7daybyyear$x)
  dl8 <- (sddl8 * 100)/meandl8
  return(dl8)
}