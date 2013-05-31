#' Function to return a list of the MA4-MA11 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MA4; Standard deviation of the percentiles of the entire flow record divided by the mean of percentiles.  
#' Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 75th,
#' 80th, 85th, 90th, and  95th percentiles for the entire flow record.  Percentiles are computed by 
#' interpolating between the ordered (ascending) flow values.  Compute the standard deviation 
#' and mean for the percentile values.  Divide the standard deviation by the mean to get MA4.  (percent-spatial)
#' MA5; 
#' The skewness of the entire flow record is computed as the mean for the entire flow record (MA1) divided by the median 
#' (MA2) for the entire flow record (dimensionless-spatial). MA6; Range in daily flows is the ratio of the 10-percent 
#' to 90-percent exceedence values for the entire flow record. Compute the 5-percent to 95-percent exceedence values 
#' for the entire flow record. Exceedence is computed by interpolating between the ordered (descending) flow values. 
#' Divide the 10-percent exceedence value by the 90-percent value (dimensionless-spatial). MA7; Range in daily flows is 
#' computed like MA6 except using the 20-percent and 80-percent exceedence values. Divide the 20-percent exceedence 
#' value by the 80-percent value (dimensionless-spatial).
#' MA8; Range in daily flows is computed like MA6 except using the 25-percent and 75-percent exceedence values. 
#' Divide the 25-percent exceedence value by the 75-percent exceedence value (dimensionless-spatial). MA9; Spread in 
#' daily flows is the ratio of the difference between the 90th and 10th percentile of the flow data to median of the 
#' entire flow record.  Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 
#' 75th, 80th, 85th, 90th, and 95th percentiles for the entire flow record.  Percentiles are computed by interpolating 
#' between the ordered (ascending) flow values.  Compute MA9 as (90th-10th) /MA2 (dimensionless-spatial). MA10; Spread 
#' in daily flows is computed like MA9 except using the 20th and 80th percentiles (dimensionless-spatial). MA11; 
#' Spread in daily flows is computed like MA9 except using the 25th and 75th percentiles (dimensionless-spatial).
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
  ma4 <- (sdev/mean)*100
  ma5 <- ma1(x)/ma2(x)
  ma6 <- percentiles[2]/percentiles[18]
  ma7 <- percentiles[4]/percentiles[16]
  ma8 <- percentiles[5]/percentiles[15]
  ma9 <- (percentiles[2]-percentiles[18])/ma2(x)
  ma10 <- (percentiles[4]-percentiles[16])/(ma2(x))
  ma11 <- (percentiles[5]-percentiles[15])/(ma2(x))
  ma4.11 <- list(ma4,ma5,ma6,ma7,ma8,ma9,ma10,ma11)
  return(ma4.11)
}