#' Function to return the DL5 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL5; Annual minimum of 90-day moving average flow. Compute the minimum of a 90-day moving average flow 
#' for each year. DL5 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl5 numeric containing DL5 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl5(qfiletempf)
dl5 <- function(qfiletempf, pref = "mean") {
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min90daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day90mean <- rollmean(subsetyr$discharge, 90, align = "right", 
                         na.pad = FALSE)
    min90daybyyear[i] <- min(day90mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dl5 <- round(median(min90daybyyear),digits=2)
  }
  else {
    dl5 <- round(mean(min90daybyyear),digits=2)
  }
  return(dl5)
}