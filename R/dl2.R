#' Function to return the DL2 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL2; Annual minimum of 3-day moving average flow. Compute the minimum of a 3-day moving average flow for 
#' each year. DL2 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl2 numeric containing DL2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl2(qfiletempf)
dl2 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min3daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day3mean <- rollmean(subsetyr$discharge, 3, align = "right", 
                         na.pad = TRUE)
    min3daybyyear[i] <- min(day3mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dl2 <- round(median(min3daybyyear),digits=2)
  }
  else {
    dl2 <- round(mean(min3daybyyear),digits=2)
  }
  return(dl2)
}