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
  day1mean <- rollmean(qfiletempf$discharge, 1, align = "right", 
                       na.pad = TRUE)
  day1rollingavg <- data.frame(qfiletempf, day1mean)
  rollingavgs1day <- subset(day1rollingavg, day1rollingavg$day1mean != 
                              "NA")
  min1daybyyear <- aggregate(rollingavgs1day$day1mean, 
                             list(rollingavgs1day$year_val), min, na.rm=TRUE)
  sddl6 <- sd(min1daybyyear$x)
  dl6 <- (sddl6 * 100)/meandl6
  return(dl6)
}