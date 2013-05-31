#' Function to return the FH5 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH5; Flood frequency. Compute the average number of flow events with flows above a threshold equal to the 
#' median flow value for the entire flow record. FH5 is the average (or median - Use Preference option) number 
#' of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh5 numeric value of FH5 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh5(qfiletempf)
fh5 <- function(qfiletempf, pref = "mean") {
  hfcrit <- ma2(qfiletempf)
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
    fh5 <- median(counter)
  }
  else {
    fh5 <- mean(counter)
  }
  return(fh5)
}