#' Function to return the DL3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL3; Annual minimum of 7-day moving average flow. Compute the minimum of a 7-day moving average flow for 
#' each year. DL3 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl3 numeric containing DL3 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl3(qfiletempf)
dl3 <- function(qfiletempf, pref = "mean") {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                              "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$wy_val), min, na.rm=TRUE)
  if (pref == "median") {
    dl3 <- median(min7daybyyear$x)
  }
  else {
    dl3 <- mean(min7daybyyear$x)
  }
  return(dl3)
}