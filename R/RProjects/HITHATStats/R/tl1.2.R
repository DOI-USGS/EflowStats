#' Function to return the TL1 and TL2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean and variability of the Julian date of the annual minimum flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return tl1.2 list containing the mean and variability of the Julian date of the annual minimum flow  for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' tl1.2(qfiletempf)
tl1.2 <- function(qfiletempf, pref = "mean") {
  min1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$wy_val), min, na.rm=TRUE)
  colnames(min1daybyyear) <- c("wy_val","discharge")
  minjulbyyear <- aggregate(qfiletempf$jul_val, list(qfiletempf$wy_val,qfiletempf$discharge),min,na.rm=TRUE)
  colnames(minjulbyyear) <- c("wy_val","discharge","jul_val")
  minjulday <- merge(minjulbyyear,min1daybyyear,by = c("wy_val","discharge"))
  meantl2<-mean(minjulday$jul_val)
  sddtl2<-sd(minjulday$jul_val)
  tl2<-(sddtl2*100)/meantl2
  if (pref == "median") {
    tl1 <- median(minjulday$jul_val)
  }
  else {
    tl1 <- mean(minjulday$jul_val)
  }
  tl1.2<-list(tl1=tl1,tl2=tl2)
  return(tl1.2)
}