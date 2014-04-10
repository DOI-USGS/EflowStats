#' Function to return the FH1 and FH2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH1; High flood pulse count. Compute the average number of flow events with flows above a threshold equal to 
#' the 75th percentile value for the entire flow record. FH1 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal). 
#' FH2; Variability in high pulse count. Compute the standard deviation in the annual pulse counts for FH1. FH2 
#' is 100 times the standard deviation divided by the mean pulse count (number of events/year-spatial). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh1.2 list of FH1 and FH2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh1.2(qfiletempf)
fh1.2 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.25))
  hfcrit <- sortq[frank]
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
  stdevfh1 <- sd(counter)
  fh2 <- round((stdevfh1 * 100)/mean(counter),digits=2)
  if (pref == "median") {
    fh1 <- round(median(counter),digits=2)
  }
  else {
    fh1 <- round(mean(counter),digits=2)
  }
  fh1.2<-list(fh1=fh1,fh2=fh2)
  return(fh1.2)
}