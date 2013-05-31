#' Function to return the DL2 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL2; Annual minimum of 3-day moving average flow. Compute the minimum of a 3-day moving average flow for 
#' each year. DL2 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl2 numeric containing DL2 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl2(qfiletempf)
dl2 <- function(qfiletempf, pref = "mean") {
  day3mean <- rollmean(qfiletempf$discharge, 3, align = "right", 
                       na.pad = TRUE)
  day3rollingavg <- data.frame(qfiletempf, day3mean)
  rollingavgs3day <- subset(day3rollingavg, day3rollingavg$day3mean != 
                              "NA")
  min3daybyyear <- aggregate(rollingavgs3day$day3mean, 
                             list(rollingavgs3day$wy_val), min, na.rm=TRUE)
  if (pref == "median") {
    dl2 <- median(min3daybyyear$x)
  }
  else {
    dl2 <- mean(min3daybyyear$x)
  }
  return(dl2)
}