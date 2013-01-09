#' Function to return the DL8 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual minimum 7-day average flows for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl8 numeric containing the variability of the annual minimum 7-day average flows for the given data frame
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
                             list(rollingavgs7day$year_val), min, na.rm=TRUE)
  sddl8 <- sd(min7daybyyear$x)
  dl8 <- (sddl8 * 100)/meandl8
  return(dl8)
}