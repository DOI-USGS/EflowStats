#' Function to return the DL3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL3; Annual minimum of 7-day moving average flow. Compute the minimum of a 7-day moving average flow for 
#' each year. DL3 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl3 numeric containing DL3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl3(qfiletempf)
dl3 <- function(qfiletempf, pref = "mean") {
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min7daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day7mean <- rollmean(subsetyr$discharge, 7, align = "right", 
                         na.pad = TRUE)
    min7daybyyear[i] <- min(day7mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dl3 <- median(min7daybyyear)
  }
  else {
    dl3 <- mean(min7daybyyear)
  }
  return(dl3)
}