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
#' @param x numeric vector of flow values.
#' @return A named numeric vector containing the MA4 through MA11 flow statistics.
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma4.11(qfiletempf)
ma4.11<-function(x) {
        
  x <- sort(x)
  
  percentiles <- quantile(x,probs=seq(0.05,0.95,0.05),type=6)
  percMean <- mean(percentiles,na.rm=TRUE)
  percSD <- sd(percentiles,na.rm=TRUE)
          
  ma4 <- round(percMean/percSD*100,digits=2)
  
  ma5 <- round(ma1(x)/ma2(x),digits=2)
  
  ma6 <- round(as.numeric(percentiles["90%"]/percentiles["10%"]), digits=2)
          
          round(quantile(sortq,.90,type=6)/
                       quantile(sortq,.1,type=6),
               digits=2)
  
  ma7 <- round(quantile(sortq,.80,type=6)/
                       quantile(sortq,.2,type=6),
               digits=2)
  
  ma8 <- round(quantile(sortq,.75,type=6)/
                       quantile(sortq,.25,type=6),
               digits=2)
  
  ma9 <- round((percentiles[2]-percentiles[18])/ma2(x),digits=2)
  ma10 <- round((percentiles[4]-percentiles[16])/(ma2(x)),digits=2)
  ma11 <- round((percentiles[5]-percentiles[15])/(ma2(x)),digits=2)
  ma4.11 <- list(ma4,ma5,ma6,ma7,ma8,ma9,ma10,ma11)
  return(ma4.11)
}