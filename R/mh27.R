#' Function to return the MH27 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH27. High peak flow. Compute the average peak flow value for flow events above a 
#' threshold equal to the 75th percentile value for for the entire record. MH27 is the average peak flow divided by 
#' the median flow for the entire record.
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh27 numeric value of the mean daily flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' mh27(qfiletempf)
mh27 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank75 <- floor(findrank(length(sortq),0.25))
  hfcrit75 <- sortq[frank75]
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
  meanex <- mean(peak_sub)
  mh27 <- meanex/ma2(qfiletempf)
  return(mh27)
}