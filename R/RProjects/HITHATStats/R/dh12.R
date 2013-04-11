#' Function to return the DH12 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the annual 7-day moving average maximum flow divided by the median flow of the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh12 numeric containing the annual 7-day moving average maximum flow divided by the median flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh12(qfiletempf)
dh12 <- function(qfiletempf) {
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                        na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                               "NA")
  max7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$year_val), max, na.rm=TRUE)
  meanmax7day <- mean(max7daybyyear$x)
  dh12 <- meanmax7day/medianflow
  return(dh12)
}