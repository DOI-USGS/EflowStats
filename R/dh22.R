#' Function to return the DH22 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and a threshold value obtained 
#' using the peakdata and peakthresh functions and calculates 
#' DH22; Flood interval. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Determine the median number of days between flood events for each year. DH22 is the mean (or median-Use 
#' Preference option) of the yearly median number of days between flood events (days-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @return dh22 numeric containing DH22 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh22(qfiletempf, 1158)
dh22 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  dur <- data.frame(Year = rep(0,nrow(qfiletempf)), dur = rep(1,nrow(qfiletempf)))
  med_yr <- rep(0,noyrs)
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
    med_yr[i] <- median(dur_sub)
  }
  med_yr[is.na(med_yr)]<-0
  dh22 <- mean(med_yr,na.rm=TRUE)
  return(dh22)
}