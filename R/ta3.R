#' Function to return the TA3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and threshold value obtained 
#' using the peakdata and peakthresh functions and calculates 
#' TA3; Seasonal predictability of flooding. Divide years up into 2-month periods (that is, Oct-Nov, Dec-Jan, 
#' and so forth). Count the number of flood days (flow events with flows > 1.67-year flood) in each period over 
#' the entire flow record. TA3 is the maximum number of flood days in any one period divided by the total number 
#' of flood days (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @return ta3 numeric containing TA3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ta3(qfiletempf, 1158)
ta3 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  nomonyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val,qfiletempf$month_val), 
                       FUN = median, na.rm=TRUE)
  colnames(nomonyears) <- c("Year","month", "momax")
  nomonyrs <- nrow(nomonyears)
  dur <- data.frame(Year = rep(0,nrow(nomonyears)), month = rep(0,nrow(nomonyears)), dur = rep(1,nrow(nomonyears)))
  for (i in 1:nomonyrs) {
    monyears <- nomonyears[i,]
    colnames(monyears) <- c("wy_val","month_val","momax")
    subsetyr <- merge(qfiletempf,monyears,by = c("wy_val","month_val"))
    flag <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        dur$Year[i] <- subsetyr$wy_val[j]
        dur$month[i] <- subsetyr$month_val[j]
        dur$dur[i] <- nevents
      } else {flag <- 0}
    }
  }
  dur$season <- ifelse(as.numeric(dur$month)==10,10,ifelse(as.numeric(dur$month)==11,10,ifelse(as.numeric(dur$month)==12,12,ifelse(as.numeric(dur$month)==1,12,ifelse(as.numeric(dur$month)==2,2,ifelse(as.numeric(dur$month)==3,2,ifelse(as.numeric(dur$month)==4,4,ifelse(as.numeric(dur$month)==5,4,ifelse(as.numeric(dur$month)==6,6,ifelse(as.numeric(dur$month)==7,6,ifelse(as.numeric(dur$month)==8,8,ifelse(as.numeric(dur$month)==9,8,99))))))))))))
  dur <- dur[!dur$season==99,]
  num_season <- aggregate(dur$dur, list(dur$season), sum)
  ta3 <- max(num_season$x)/sum(num_season$x)
  return(ta3)
}