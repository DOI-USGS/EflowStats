#' Function to return the DH5 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of the annual maximum 30-day moving average flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh5 numeric containing the mean of the annual maximum 30-day moving average flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh5(qfiletempf)
dh5 <- function(qfiletempf, pref = "mean") {
  day90mean <- rollmean(qfiletempf$discharge, 90, align = "right", 
                        na.pad = TRUE)
  day90rollingavg <- data.frame(qfiletempf, day90mean)
  rollingavgs90day <- subset(day90rollingavg, day90rollingavg$day90mean != 
                               "NA")
  max90daybyyear <- aggregate(rollingavgs90day$day90mean, 
                              list(rollingavgs90day$year_val), max, na.rm=TRUE)
  if (pref == "median") {
    dh5 <- median(max90daybyyear$x)
  }
  else {
    dh5 <- mean(max90daybyyear$x)
  }
  return(dh5)
}