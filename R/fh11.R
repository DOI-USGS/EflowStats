#' Function to return the FH11 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and a threshold value calculated 
#' for the site using the peakdata and peakthresh functions and calculates 
#' FH11; Flood frequency. Compute the average number of flow events with flows above a threshold equal to flow 
#' corresponding to a 1.67-year recurrence interval. FH11 is the average (or median-Use Preference option) number 
#' of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @param pref string containing a "mean" or "median" preference
#' @return fh11 numeric containing FH11 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' sites<-"02178400"
#' peakValues<-getPeakData(sites)
#' thresh<-1158.495
#' fh11(qfiletempf,thresh)
fh11 <- function(qfiletempf, thresh, pref = "mean") {
  lfcrit <- thresh
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh4 <- rep(0, noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        hfcountbyyrfh4[i] <- ifelse(flag==1,hfcountbyyrfh4[i]+1,hfcountbyyrfh4[i])
      } else {flag<-0}
    }
  }
  if (pref == "median") {
    fh11 <- round(median(hfcountbyyrfh4),digits=2)
  }
  else {
    fh11 <- round(mean(hfcountbyyrfh4),digits=2)
  }
  return(fh11)
}