#' Function to return the DL5 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL5; Annual minimum of 90-day moving average flow. Compute the minimum of a 90-day moving average flow 
#' for each year. DL5 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl5 numeric containing DL5 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl5(qfiletempf)
dl5 <- function(qfiletempf, pref = "mean") {
  day90mean <- rollmean(qfiletempf$discharge, 90, align = "right", 
                        na.pad = TRUE)
  day90rollingavg <- data.frame(qfiletempf, day90mean)
  rollingavgs90day <- subset(day90rollingavg, day90rollingavg$day90mean != 
                               "NA")
  min90daybyyear <- aggregate(rollingavgs90day$day90mean, 
                              list(rollingavgs90day$wy_val), min, na.rm=TRUE)
  if (pref == "median") {
    dl5 <- median(min90daybyyear$x)
  }
  else {
    dl5 <- mean(min90daybyyear$x)
  }
  return(dl5)
}