#' Function to return the TA3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the seasonal predictability of floods above the 60th percentile for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @return ta3 numeric containing the max floods in a period over total floods for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ta3(qfiletempf)
ta3 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  nomonyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val,qfiletempf$month_val), 
                       FUN = median, na.rm=TRUE)
  colnames(nomonyears) <- c("Year","month", "momax")
  nomonyrs <- length(nomonyears$Year)
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
  num_season <- aggregate(dur$dur, list(dur$Year,dur$month), sum)
  ta3 <- max(num_season$x)/sum(num_season$x)
  return(ta3)
}