#' Function to return the DH22 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of annual median days between flood events for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @return dh22 numeric containing the mean of annual median flood intervals for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data, stringsAsFactors=FALSE)
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
        if (subsetyr$discharge[j] >= lfcrit) {
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
  dh22 <- mean(med_yr)
  return(dh22)
}