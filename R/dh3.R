#' Function to return the DH3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH3; Annual maximum of 7-day moving average flows. Compute the maximum of a 7-day moving average flow for 
#' each year. DH3 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh3 numeric containing DH3 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh3(qfiletempf)
dh3 <- function(qfiletempf, pref = "mean") {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                        na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                               "NA")
  max7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$wy_val), max, na.rm=TRUE)
  if (pref == "median") {
    dh3 <- median(max7daybyyear$x)
  }
  else {
    dh3 <- mean(max7daybyyear$x)
  }
  return(dh3)
}