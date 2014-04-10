#' Function to return the TH1 and TH2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' TH1; Julian date of annual maximum. Determine the Julian date that the maximum flow occurs for each year. 
#' Transform the dates to relative values on a circular scale (radians or degrees). Compute the x and y components 
#' for each year and average them across all years. Compute the mean angle as the arc tangent of y-mean divided by 
#' x-mean. Transform the resultant angle back to Julian date (Julian day-spatial). and 
#' TH2; Variability in Julian date of annual maxima. Compute the coefficient of variation for the mean x and y 
#' components and convert to a date (Julian days-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return th1.2 list containing TH1 and TH2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' th1.2(qfiletempf)
th1.2 <- function(qfiletempf) {
  max1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$wy_val), max, na.rm=TRUE)
  colnames(max1daybyyear) <- c("wy_val","discharge")
  qfiletempf$wy_day_val <- ifelse(qfiletempf$jul_val>=274,qfiletempf$jul_val-273,qfiletempf$jul_val+92)
  maxjulbyyear <- aggregate(qfiletempf$wy_day_val, list(qfiletempf$wy_val,qfiletempf$discharge),min,na.rm=TRUE)
  colnames(maxjulbyyear) <- c("wy_val","discharge","jul_val")
  maxjulday <- merge(maxjulbyyear,max1daybyyear,by = c("wy_val","discharge"))
  maxjulday$jul_val <- ifelse(maxjulday$jul_val<=92,maxjulday$jul_val+274,maxjulday$jul_val-92)
  maxjulday$np <- cos(maxjulday$jul_val*2*pi/365.25)
  maxjulday$mdata <- sin(maxjulday$jul_val*2*pi/365.25)
  xbar <- mean(maxjulday$np)
  ybar <- mean(maxjulday$mdata)
  if (xbar>0) {
    th1_temp <- atan(ybar/xbar)*180/pi
  } else if (xbar<0) {
    th1_temp <- (atan(ybar/xbar)*180/pi)+180
  } else if (xbar==0 && ybar>0) {
    th1_temp <- 90
  } else if (xbar==0 && ybar<0) {
    th1_temp <- 270
  }
  th1_temp <- ifelse(th1_temp<0,th1_temp+360,th1_temp)
  th1 <- round(th1_temp*365.25/360,digits=2)
  th2_a <- sqrt((xbar*xbar)+(ybar*ybar))
  th2_b <- sqrt(2*(1-th2_a))
  th2 <- round(th2_b*180/pi/360*365.25,digits=2)
  th1.2<-list(th1=th1,th2=th2)
  return(th1.2)
}