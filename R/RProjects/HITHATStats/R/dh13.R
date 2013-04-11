#' Function to return the DH13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the annual 30-day moving average maximum flow divided by the median flow of the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh12 numeric containing the annual 30-day moving average maximum flow divided by the median flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh13(qfiletempf)
dh13 <- function(qfiletempf) {
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  day30mean <- rollmean(qfiletempf$discharge, 30, align = "right", 
                        na.pad = TRUE)
  day30rollingavg <- data.frame(qfiletempf, day30mean)
  rollingavgs30day <- subset(day30rollingavg, day30rollingavg$day30mean != 
                               "NA")
  max30daybyyear <- aggregate(rollingavgs30day$day30mean, 
                              list(rollingavgs30day$year_val), max, na.rm=TRUE)
  meanmax30day <- mean(max30daybyyear$x)
  dh13 <- meanmax30day/medianflow
  return(dh13)
}