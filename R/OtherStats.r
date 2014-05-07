#' Function to return the OtherStats statistics for a given data series
#' This is a function to compute the 7 statistics of daily streamflow. Input to the function is a
#' time series of streamflow with date in the format Y-m-d. Output is a vector of OtherStats 
#' 1) mean, 2) median, 3) cv, 4) cv daily, 5) l7Q10, 6) l7Q2, 7) return_10 and 8) flow percentiles.
#' 
#' @param timeseries1 data frame of daily flow data
#' @return OtherStats data frame of calculated statistics
#' @export
#' @examples
#' timeseries1<-sampleData
#' OtherStats(timeseries1)
OtherStats<-function(timeseries1)  {

  sdbyyr <- aggregate(data$discharge, list(data$wy_val), 
                      sd)
  colnames(sdbyyr) <- c("Year", "sdq")
  meanbyyr <- aggregate(data$discharge, list(data$wy_val), 
                        mean, na.rm=TRUE)
  colnames(meanbyyr) <- c("Year", "meanq")
  medbyyr <- aggregate(data$discharge, list(data$wy_val), 
                       median, na.rm=TRUE)
  colnames(medbyyr) <- c("Year","medq")
  dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
                         meanbyyr$meanq, medbyyr$medq,stringsAsFactors=FALSE)
  colnames(dfcvbyyr) <- c("Year", "sdq", "meanq", "medq")
  cvbyyr <- dfcvbyyr$sdq/dfcvbyyr$meanq
  dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr, stringsAsFactors=FALSE)
  colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", "medq", 
                           "cvq")
  
  mean_flow<-round(mean(dfcvbyyrf$meanq,na.rm=TRUE),digits=2)
  med_flow<-round(median(dfcvbyyrf$meanq,na.rm=TRUE),digits=2)
  cv_flow<-round(cv(dfcvbyyrf$meanq),digits=2)
  cv_daily<-round(cv(data$discharge),digits=2)
  l7Q10v<-l7Q10(data)
  l7Q2v<-l7Q2(data)
  return_10v<-return_10(data)
  
  obs_percentiles <- flow_perc(data$discharge)
  flow_10 <- obs_percentiles[1]
  flow_25 <- obs_percentiles[2]
  flow_50 <- obs_percentiles[3]
  flow_75 <- obs_percentiles[4]
  flow_90 <- obs_percentiles[5]
  flow_15 <- obs_percentiles[6]
  OtherStats <- c(med_flow,cv_flow,cv_daily,l7Q10v,l7Q2v,return_10v,flow_10,flow_25,flow_50,flow_75,flow_90,flow_15)
  return(OtherStats)
}
