#' Function to return the DH24 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and a threshold value obtained 
#' using the peakdata and peakthresh functions and calculates 
#' DH24; Flood-free days. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Compute the maximum number of days that the flow is below the threshold for each year. DH24 is the mean 
#' (or median-Use Preference option) of the maximum yearly no-flood days (days-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @return dh24 numeric containing DH24 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh24(qfiletempf, 1158)
dh24 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  dur <- data.frame(Year = rep(0,nrow(qfiletempf)), dur = rep(1,nrow(qfiletempf)))
  max_yr <- rep(0,noyrs)
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
      } else {
        if (flag > 0) {
          dur$dur[nevents]<-pdur
          dur$Year[nevents]<-subsetyr$wy_val[j]
        }
        flag <- 0
        pdur <- 0
      }
    }
    dur_sub <- dur$dur[dur$Year==subsetyr$wy_val[j]]
    max_yr[i] <- ifelse(length(dur_sub)>0,max(dur_sub),0)
  }
  max_yr[max_yr==0]<-NA
  dh24 <- mean(max_yr,na.rm=TRUE)
  return(dh24)
}