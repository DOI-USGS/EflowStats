#' Function to return the MH25 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH25. High peak flow. Compute the average peak flow value for flow events above a 
#' threshold equal to three times the median for for the entire record. MH25 is the average peak flow divided by 
#' the median flow for the entire record.
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh25 numeric value of the mean daily flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' mh25(qfiletempf)
mh25 <- function(qfiletempf) {
  hfcrit <- 3 * ma2(qfiletempf)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  peak <- rep(0,nrow(qfiletempf))
  nevents <- 1
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    peak[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        temp <- subsetyr$discharge[j]
        nevents <- ifelse(flag==1,nevents+1,nevents)
        peak[nevents] <- ifelse(temp>peak[nevents],temp,peak[nevents])
      } else {flag <- 0}
    }
  }
  peak_sub <- subset(peak,peak>0)
  meanex <- mean(peak_sub)
  mh25 <- meanex/ma2(qfiletempf)
  return(mh25)
}