#' Function to return the DL7 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual minimum 3-day average flows for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl7 numeric containing the variability of the annual minimum 3-day average flows for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl7(qfiletempf)
dl7 <- function(qfiletempf) {
  meandl7 <- dl2(qfiletempf, pref = "mean")
  day3mean <- rollmean(qfiletempf$discharge, 3, align = "right", 
                        na.pad = TRUE)
  day3rollingavg <- data.frame(qfiletempf, day3mean)
  rollingavgs3day <- subset(day3rollingavg, day3rollingavg$day3mean != 
                               "NA")
  min3daybyyear <- aggregate(rollingavgs3day$day3mean, 
                              list(rollingavgs3day$year_val), min, na.rm=TRUE)
  sddl7 <- sd(min3daybyyear$x)
  dl7 <- (sddl7 * 100)/meandl7
  return(dl7)
}