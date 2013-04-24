#' Function to return the DL10 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual minimum 90-day average flows for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl10 numeric containing the variability of the annual minimum 90-day average flows for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl10(qfiletempf)
dl10 <- function(qfiletempf) {
  meandl10 <- dl5(qfiletempf, pref = "mean")
  day90mean <- rollmean(qfiletempf$discharge, 90, align = "right", 
                        na.pad = TRUE)
  day90rollingavg <- data.frame(qfiletempf, day90mean)
  rollingavgs90day <- subset(day90rollingavg, day90rollingavg$day90mean != 
                               "NA")
  min90daybyyear <- aggregate(rollingavgs90day$day90mean, 
                              list(rollingavgs90day$wy_val), min, na.rm=TRUE)
  sddl10 <- sd(min90daybyyear$x)
  dl10 <- (sddl10 * 100)/meandl10
  return(dl10)
}