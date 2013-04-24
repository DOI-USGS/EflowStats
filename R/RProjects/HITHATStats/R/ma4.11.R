#' Function to return a list of the MA4-MA11 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MA4; standard deviation of the percentiles of the logs of the entire flow record 
#' divided by the mean of percentiles of the logs. Compute the log10 of the daily flows for the 
#' entire record. Compute the 5th,10th,15th,20th,30th,35th,40th,45th,50th,55th,60th,65th,70th,
#' 75th,80th,85th,90th,95th percentiles for the log10s of the flow record. Percentiles are computed 
#' by interpolating between the ordered (ascending) log10s of the flow values. Compute the standard 
#' deviation and mean for the percentile vaules. Divide the standard deviation by the mean. MA5; 
#' Skewness of the entire flow record. Mean (MA1) divided by median (MA2). MA6; Range in daily flows 
#' is the ratio of the 10% to 90% exceedence values for the entire flow record. Compute the 5 to 95 
#' percent exceedence values. Exceedence is computed by interpolating between the ordered (descending) 
#' values. Divide the 10% exceedence value by the 90% value. MA7; Same as MA6, for 20% and 80% exceedence values.
#' MA8; Same as MA6, for 25% and 75% exceedence values. MA9; Spread in daily flows is the ratio of the 
#' difference between the 90th and 10th percentile of the logs of the flow data to the log of the median 
#' of the entire flow record. MA10; Same as MA9, but with 20th and 80th percentiles. MA11; 
#' Same as MA9, but with 25th and 75th percentiles.
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ma4.11 list of the MA4-MA11 statistics for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' ma4.11(x)
ma4.11<-function(x) {
  isolateq <- x$discharge
  sortq <- sort(isolateq)
  percentiles<-vector(length=19)
  percentiles[1] <- sortq[floor(findrank(length(sortq), 0.05))]  
  percentiles[2] <- sortq[floor(findrank(length(sortq), 0.1))]
  percentiles[3] <- sortq[floor(findrank(length(sortq), 0.15))]
  percentiles[4] <- sortq[floor(findrank(length(sortq), 0.2))]
  percentiles[5] <- sortq[floor(findrank(length(sortq), 0.25))]
  percentiles[6] <- sortq[floor(findrank(length(sortq), 0.3))]
  percentiles[7] <- sortq[floor(findrank(length(sortq), 0.35))]
  percentiles[8] <- sortq[floor(findrank(length(sortq), 0.4))]
  percentiles[9] <- sortq[floor(findrank(length(sortq), 0.45))]
  percentiles[10] <- sortq[floor(findrank(length(sortq), 0.5))]
  percentiles[11] <- sortq[floor(findrank(length(sortq), 0.55))]
  percentiles[12] <- sortq[floor(findrank(length(sortq), 0.6))]
  percentiles[13] <- sortq[floor(findrank(length(sortq), 0.65))]
  percentiles[14] <- sortq[floor(findrank(length(sortq), 0.7))]
  percentiles[15] <- sortq[floor(findrank(length(sortq), 0.75))]
  percentiles[16] <- sortq[floor(findrank(length(sortq), 0.8))]
  percentiles[17] <- sortq[floor(findrank(length(sortq), 0.85))]
  percentiles[18] <- sortq[floor(findrank(length(sortq), 0.9))]
  percentiles[19] <- sortq[floor(findrank(length(sortq), 0.95))]
  mean <- mean(percentiles,na.rm=TRUE)
  sdev <- sd(percentiles, na.rm=TRUE)
  ma4 <- sdev/mean*100
  ma5 <- ma1(x)/ma2(x)
  ma6 <- percentiles[2]/percentiles[18]
  ma7 <- percentiles[4]/percentiles[16]
  ma8 <- percentiles[5]/percentiles[15]
  sortqLog <- sort(log10(isolateq))
  percentilesLog<-vector(length=19)
  percentilesLog[1] <- sortqLog[floor(findrank(length(sortq), 0.05))]  
  percentilesLog[2] <- sortqLog[floor(findrank(length(sortq), 0.1))]
  percentilesLog[3] <- sortqLog[floor(findrank(length(sortq), 0.15))]
  percentilesLog[4] <- sortqLog[floor(findrank(length(sortq), 0.2))]
  percentilesLog[5] <- sortqLog[floor(findrank(length(sortq), 0.25))]
  percentilesLog[6] <- sortqLog[floor(findrank(length(sortq), 0.3))]
  percentilesLog[7] <- sortqLog[floor(findrank(length(sortq), 0.35))]
  percentilesLog[8] <- sortqLog[floor(findrank(length(sortq), 0.4))]
  percentilesLog[9] <- sortqLog[floor(findrank(length(sortq), 0.45))]
  percentilesLog[10] <- sortqLog[floor(findrank(length(sortq), 0.5))]
  percentilesLog[11] <- sortqLog[floor(findrank(length(sortq), 0.55))]
  percentilesLog[12] <- sortqLog[floor(findrank(length(sortq), 0.6))]
  percentilesLog[13] <- sortqLog[floor(findrank(length(sortq), 0.65))]
  percentilesLog[14] <- sortqLog[floor(findrank(length(sortq), 0.7))]
  percentilesLog[15] <- sortqLog[floor(findrank(length(sortq), 0.75))]
  percentilesLog[16] <- sortqLog[floor(findrank(length(sortq), 0.8))]
  percentilesLog[17] <- sortqLog[floor(findrank(length(sortq), 0.85))]
  percentilesLog[18] <- sortqLog[floor(findrank(length(sortq), 0.9))]
  percentilesLog[19] <- sortqLog[floor(findrank(length(sortq), 0.95))]
  ma9 <- (percentiles[2]-percentiles[18])/ma2(x)
  ma10 <- (percentiles[4]-percentiles[16])/(ma2(x))
  ma11 <- (percentiles[5]-percentiles[15])/(ma2(x))
  ma4.11 <- list(ma4,ma5,ma6,ma7,ma8,ma9,ma10,ma11)
  return(ma4.11)
}