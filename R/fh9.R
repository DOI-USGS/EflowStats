#' Function to return the FH9 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH9; Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' 75-percent exceedence value for the entire flow record. FH9 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh9 numeric value of FH9 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh9(qfiletempf)
fh9 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  hfcrit <- quantile(sortq,.25,type=6)
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
    fh9 <- median(counter)
  }
  else {
    fh9 <- mean(counter)
  }
  return(fh9)
}