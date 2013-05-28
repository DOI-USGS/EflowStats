#' Function to return the TL1 and TL2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean and variability of the Julian date of the annual minimum flow for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return tl1.2 list containing the mean and variability of the Julian date of the annual minimum flow  for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' tl1.2(qfiletempf)
tl1.2 <- function(qfiletempf) {
  min1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$wy_val), min, na.rm=TRUE)
  colnames(min1daybyyear) <- c("wy_val","discharge")
  qfiletempf$wy_day_val <- ifelse(qfiletempf$jul_val>=274,qfiletempf$jul_val-273,qfiletempf$jul_val+92)
  minjulbyyear <- aggregate(qfiletempf$wy_day_val, list(qfiletempf$wy_val,qfiletempf$discharge),min,na.rm=TRUE)
  colnames(minjulbyyear) <- c("wy_val","discharge","jul_val")
  minjulday <- merge(minjulbyyear,min1daybyyear,by = c("wy_val","discharge"))
  minjulday$jul_val <- ifelse(minjulday$jul_val<=92,minjulday$jul_val+274,minjulday$jul_val-92)
  minjulday$np <- cos(minjulday$jul_val*2*pi/365.25)
  minjulday$mdata <- sin(minjulday$jul_val*2*pi/365.25)
  xbar <- mean(minjulday$np)
  ybar <- mean(minjulday$mdata)
  if (xbar>0) {
    tl1_temp <- atan(ybar/xbar)*180/pi
  } else if (xbar<0) {
    tl1_temp <- (atan(ybar/xbar)*180/pi)+180
  } else if (xbar==0 && ybar>0) {
    tl1_temp <- 90
  } else if (xbar==0 && ybar<0) {
    tl1_temp <- 270
  }
  tl1_temp <- ifelse(tl1_temp<0,tl1_temp+360,tl1_temp)
  tl1 <- tl1_temp*365.25/360
  tl2_a <- sqrt((xbar*xbar)+(ybar*ybar))
  tl2_b <- sqrt(2*(1-tl2_a))
  tl2 <- tl2_b*180/pi/360*365.25
  tl1.2<-list(tl1=tl1,tl2=tl2)
  return(tl1.2)
}