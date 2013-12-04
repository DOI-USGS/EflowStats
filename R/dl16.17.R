#' Function to return the DL16 and DL17 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL16; Low flow pulse duration. Compute the average pulse duration for each year for flow events below a 
#' threshold equal to the 25th percentile value for the entire flow record. DL16 is the median of the yearly 
#' average durations (number of days-temporal). and 
#' DL17; Variability in low pulse duration. Compute the standard deviation for the yearly average low pulse durations. 
#' DL17 is 100 times the standard deviation divided by the mean of the yearly average low pulse durations 
#' (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl16.17 list containing DL16 and DL17 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl16.17(qfiletempf)
dl16.17 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  lfcrit <- quantile(sortq,.25,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  lfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]<lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {lfdur[i]<-pdur/nevents}
  }
  dl16 <- median(lfdur)
  dl17 <- (sd(lfdur)*100)/mean(lfdur)
  dl16.17 <- list(dl16=dl16,dl17=dl17)
  return(dl16.17)
}