#' Function to return the DH4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH4; Annual maximum of 30-day moving average flows. Compute the maximum of 30-day moving average flows. Compute 
#' the maximum of a 30-day moving average flow for each year. DH4 is the mean (or median-Use Preference option) 
#' of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh4 numeric containing DH4 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh4(qfiletempf)
dh4 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
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
  if (pref == "median") {
    dh4 <- round(median(max30daybyyear),digits=2)
  }
  else {
    dh4 <- round(mean(max30daybyyear),digits=2)
  }
  return(dh4)
}