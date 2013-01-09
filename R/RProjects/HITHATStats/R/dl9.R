#' Function to return the DL9 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual minimum 30-day average flows for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl9 numeric containing the variability of the annual minimum 30-day average flows for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl9(qfiletempf)
dl9 <- function(qfiletempf) {
  meandl9 <- dl4(qfiletempf, pref = "mean")
  day30mean <- rollmean(qfiletempf$discharge, 30, align = "right", 
                        na.pad = TRUE)
  day30rollingavg <- data.frame(qfiletempf, day30mean)
  rollingavgs30day <- subset(day30rollingavg, day30rollingavg$day30mean != 
                               "NA")
  min30daybyyear <- aggregate(rollingavgs30day$day30mean, 
                              list(rollingavgs30day$year_val), min, na.rm=TRUE)
  sddl9 <- sd(min30daybyyear$x)
  dl9 <- (sddl9 * 100)/meandl9
  return(dl9)
}