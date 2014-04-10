#' Function to return the FH6 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH6; Flood frequency. Compute the average number of flow events with flows above a threshold equal to three 
#' times the median flow value for the entire flow record. FH6 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh6 numeric value of FH6 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh6(qfiletempf)
fh6 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 3*ma2(qfiletempf)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  if (pref == "median") {
    fh6 <- round(median(counter),digits=2)
  }
  else {
    fh6 <- round(mean(counter),digits=2)
  }
  return(fh6)
}