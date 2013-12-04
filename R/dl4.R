#' Function to return the DL4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL4; Annual minimum of 30-day moving average flow. Compute the minimum of a 30-day moving average flow 
#' for each year. DL4 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl4 numeric containing DL4 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl4(qfiletempf)
dl4 <- function(qfiletempf, pref = "mean") {
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min30daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day30mean <- rollmean(subsetyr$discharge, 30, align = "right", 
                         na.pad = TRUE)
    min30daybyyear[i] <- min(day30mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dl4 <- median(min30daybyyear)
  }
  else {
    dl4 <- mean(min30daybyyear)
  }
  return(dl4)
}