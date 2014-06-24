#' Function to return the DH2 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH2; Annual maximum of 3-day moving average flows. Compute the maximum of a 3-day moving average flow for 
#' each year. DH2 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh2 numeric containing DH2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh2(qfiletempf)
dh2 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max3daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day3mean <- rollmean(subsetyr$discharge, 3, align = "right", 
                         na.pad = TRUE)
    max3daybyyear[i] <- max(day3mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dh2 <- round(median(max3daybyyear),digits=2)
  }
  else {
    dh2 <- round(mean(max3daybyyear),digits=2)
  }
  return(dh2)
}