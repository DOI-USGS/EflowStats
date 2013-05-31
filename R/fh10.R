#' Function to return the FH10 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH10; Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' median of the annual minima for the entire flow record. FH10 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh10 numeric value of FH10 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh10(qfiletempf)
fh10 <- function(qfiletempf, pref = "mean") {
  minbyyear <- aggregate(qfiletempf$discharge,list(qfiletempf$wy_val),FUN=min,na.rm=TRUE)
  medmin <- median(minbyyear$x)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh4 <- rep(0, noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>medmin) {
        flag <- flag+1
        hfcountbyyrfh4[i] <- ifelse(flag==1,hfcountbyyrfh4[i]+1,hfcountbyyrfh4[i])
      } else {flag<-0}
    }
  }
  if (pref == "median") {
    fh10 <- median(hfcountbyyrfh4)
  }
  else {
    fh10 <- mean(hfcountbyyrfh4)
  }
  return(fh10)
}