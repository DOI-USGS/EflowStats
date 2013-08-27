#' Function to return the DH10 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH10; Variability of annual maximum of 90-day moving average flows. Compute the standard deviation for the 
#' maximum 90-day moving averages. DH10 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh10 numeric containing DH10 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh10(qfiletempf)
dh10 <- function(qfiletempf) {
  meandh10 <- dh5(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max90daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day90mean <- rollmean(subsetyr$discharge, 90, align = "right", 
                          na.pad = TRUE)
    max90daybyyear[i] <- max(day90mean, na.rm=TRUE)
  }
  sddh10 <- sd(max90daybyyear)
  dh10 <- (sddh10 * 100)/meandh10
  return(dh10)
}