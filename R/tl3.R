#' Function to return the TL3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and a threshold value obtained 
#' using the peakdata and getPeakThresh functions and calculates 
#' TL3; Seasonal predictability of low flow. Divide years up into 2-month periods (that is, Oct-Nov, Dec-Jan, and 
#' so forth). Count the number of low flow events (flow events with flows ??? 5 year flood threshold) in each period 
#' over the entire flow record. TL3 is the maximum number of low flow events in any one period divided by the total 
#' number of low flow events (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh value containing the 5-year recurrence value for the site
#' @return tl3 numeric containing TL3 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' tl3(qfiletempf, 1161.38)
tl3 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  qfiletempf$season <- ifelse(as.numeric(qfiletempf$month)==10,10,ifelse(as.numeric(qfiletempf$month)==11,10,ifelse(as.numeric(qfiletempf$month)==12,12,ifelse(as.numeric(qfiletempf$month)==1,12,ifelse(as.numeric(qfiletempf$month)==2,2,ifelse(as.numeric(qfiletempf$month)==3,2,ifelse(as.numeric(qfiletempf$month)==4,4,ifelse(as.numeric(qfiletempf$month)==5,4,ifelse(as.numeric(qfiletempf$month)==6,6,ifelse(as.numeric(qfiletempf$month)==7,6,ifelse(as.numeric(qfiletempf$month)==8,8,ifelse(as.numeric(qfiletempf$month)==9,8,99))))))))))))
  nomonyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val,qfiletempf$season), 
                          FUN = median, na.rm=TRUE)
  colnames(nomonyears) <- c("Year","season", "momax")
  nomonyrs <- length(nomonyears$Year)
  dur <- data.frame(Year = rep(0,nrow(nomonyears)), season = rep(0,nrow(nomonyears)), dur = rep(1,nrow(nomonyears)))
  nlf <- rep(0,12)
  tnlf <- 0
  for (i in 1:nomonyrs) {
    monyears <- nomonyears[i,]
    colnames(monyears) <- c("wy_val","season","momax")
    subsetyr <- merge(qfiletempf,monyears,by = c("wy_val","season"))
    flag <- 0
    tnlf <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]<lfcrit) {
        flag <- flag+1
        tnlf <- ifelse(flag==1,tnlf+1,tnlf)
        nlf[subsetyr$season] <- ifelse(flag==1,nlf[subsetyr$season]+1,nlf[subsetyr$season])
        dur$Year[i] <- subsetyr$wy_val[j]
        dur$season[i] <- subsetyr$season[j]
        dur$dur[i] <- tnlf
      } else {flag <- 0}
    }
  }
  dur <- dur[!dur$season==99,]
  num_season <- aggregate(dur$dur, list(dur$season), sum)
  tl3 <- max(num_season$x)/sum(num_season$x)
  return(tl3)
}