#' Function to return the MH27 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH27, high peak flow. Compute the average peak flow value for flow events above a threshold equal 
#' to 75th percentile value for the entire flow record. MH27 is the average peak flow divided by the median flow 
#' for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh27 numeric value of MH27 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh27(qfiletempf)
mh27 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  hfcrit75 <- quantile(sortq,.75,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  peak <- rep(0,nrow(qfiletempf))
  nevents <- 0
  flag <- 0
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit75) {
        flag <- flag+1
        temp <- subsetyr$discharge[j]
        nevents <- ifelse(flag==1,nevents+1,nevents)
        peak[nevents] <- ifelse(temp>peak[nevents],temp,peak[nevents])
      } else {flag <- 0}
    }
  }
  peak_sub <- subset(peak,peak>0)
  meanex <- ifelse(length(peak_sub)==0,0,mean(peak_sub))
  mh27 <- meanex/ma2(qfiletempf)
  return(mh27)
}