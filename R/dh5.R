#' Function to return the DH5 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH5; Annual maximum of 90-day moving average flows. Compute the maximum of a 90-day moving average flow for 
#' each year. DH5 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh5 numeric containing DH5 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh5(qfiletempf)
dh5 <- function(qfiletempf, pref = "mean") {
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
  if (pref == "median") {
    dh5 <- median(max90daybyyear)
  }
  else {
    dh5 <- mean(max90daybyyear)
  }
  return(dh5)
}