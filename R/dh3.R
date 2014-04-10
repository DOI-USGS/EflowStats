#' Function to return the DH3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH3; Annual maximum of 7-day moving average flows. Compute the maximum of a 7-day moving average flow for 
#' each year. DH3 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh3 numeric containing DH3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh3(qfiletempf)
dh3 <- function(qfiletempf, pref = "mean") {
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max7daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day7mean <- rollmean(subsetyr$discharge, 7, align = "right", 
                         na.pad = FALSE)
    max7daybyyear[i] <- max(day7mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dh3 <- round(median(max7daybyyear),digits=2)
  }
  else {
    dh3 <- round(mean(max7daybyyear),digits=2)
  }
  return(dh3)
}