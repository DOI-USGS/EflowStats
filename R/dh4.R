#' Function to return the DH4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH4; Annual maximum of 30-day moving average flows. Compute the maximum of 30-day moving average flows. Compute 
#' the maximum of a 30-day moving average flow for each year. DH4 is the mean (or median-Use Preference option) 
#' of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh4 numeric containing DH4 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh4(qfiletempf)
dh4 <- function(qfiletempf, pref = "mean") {
  day30mean <- rollmean(qfiletempf$discharge, 30, align = "right", 
                        na.pad = TRUE)
  day30rollingavg <- data.frame(qfiletempf, day30mean)
  rollingavgs30day <- subset(day30rollingavg, day30rollingavg$day30mean != 
                               "NA")
  max30daybyyear <- aggregate(rollingavgs30day$day30mean, 
                              list(rollingavgs30day$wy_val), max, na.rm=TRUE)
  if (pref == "median") {
    dh4 <- median(max30daybyyear$x)
  }
  else {
    dh4 <- mean(max30daybyyear$x)
  }
  return(dh4)
}