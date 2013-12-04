#' Function to return the TH1 and TH2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean and variability of the Julian date of the annual maximum flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return th1.2 list containing the mean and variability of the Julian date of the annual maximum flow  for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' th1.2(qfiletempf)
th1.2 <- function(qfiletempf, pref = "mean") {
  max1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$wy_val), max, na.rm=TRUE)
  colnames(max1daybyyear) <- c("wy_val","discharge")
  maxjulbyyear <- aggregate(qfiletempf$jul_val, list(qfiletempf$wy_val,qfiletempf$discharge),max,na.rm=TRUE)
  colnames(maxjulbyyear) <- c("wy_val","discharge","jul_val")
  maxjulday <- merge(maxjulbyyear,max1daybyyear,by = c("wy_val","discharge"))
  meanth2<-mean(maxjulday$jul_val)
  sddth2<-sd(maxjulday$jul_val)
  th2<-(sddth2*100)/meanth2
  if (pref == "median") {
    th1 <- median(maxjulday$jul_val)
  }
  else {
    th1 <- mean(maxjulday$jul_val)
  }
  th1.2<-list(th1=th1,th2=th2)
  return(th1.2)
}