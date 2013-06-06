#' Function to return the DH9 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH9; Variability of annual maximum of 30-day moving average flows. Compute the standard deviation for the 
#' maximum 30-day moving averages. DH9 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh9 numeric containing DH9 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh9(qfiletempf)
dh9 <- function(qfiletempf) {
  meandh9 <- dh4(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max30daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day30mean <- rollmean(subsetyr$discharge, 30, align = "right", 
                          na.pad = TRUE)
    max30daybyyear[i] <- max(day30mean, na.rm=TRUE)
  }
  sddh9 <- sd(max30daybyyear)
  dh9 <- (sddh9 * 100)/meandh9
  return(dh9)
}