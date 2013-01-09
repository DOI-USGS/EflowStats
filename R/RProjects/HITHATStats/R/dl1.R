#' Function to return the DL1 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of the annual minimum daily flows for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl1 numeric containing the mean of the annual minimum daily flows for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl1(qfiletempf)
dl1 <- function(qfiletempf, pref = "mean") {
  day1mean <- rollmean(qfiletempf$discharge, 1, align = "right", 
                       na.pad = TRUE)
  day1rollingavg <- data.frame(qfiletempf, day1mean)
  rollingavgs1day <- subset(day1rollingavg, day1rollingavg$day1mean != 
                              "NA")
  min1daybyyear <- aggregate(rollingavgs1day$day1mean, 
                             list(rollingavgs1day$year_val), min, na.rm=TRUE)
  if (pref == "median") {
    dl1 <- median(min1daybyyear$x)
  }
  else {
    dl1 <- mean(min1daybyyear$x)
  }
  return(dl1)
}