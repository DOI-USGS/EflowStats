#' Function to compute the seasonal factors (amplitude and phase) for a given data series
#' 
#' This function accepts a data frame containing daily streamflow data, then computes seasonality 
#' variables by first standardizing flows, the fitting relation 
#' A*cos(2*pi*t) + B*sin(2*pi*t)1) Get decimal yearand returns the amplitude and phase
#' 
#' @param data data frame containing daily discharge data
#' @return seasonality vector of seasonal factors (amplitude and phase)
#' @export
#' @examples
#' qfiletempf<-sampleData
#' seasonality(qfiletempf)
seasonality <- function(data) {
  rawdates<-timeseries$date
  dateaschar<-as.character(rawdates)
  jday<-strptime(timeseries$date, "%Y-%m-%d")$yday+1
  decimal_year<-as.numeric(timeseries$year_val)+(jday/365.25)
  #2) Standardize flows
  std_flows<-scale(timeseries$flow, center = TRUE, scale = TRUE)
  #3) Use linear model to fit 
  seasonfit<-lm(std_flows~cos(2*pi*decimal_year)+sin(2*pi*decimal_year))
  seasonA<-as.vector(seasonfit$coefficients[2])
  seasonB<-as.vector(seasonfit$coefficients[3]) 
  #Now compute the amplitude and phase of the seasonal signal
  amplitude<-round(sqrt((seasonA^2)+(seasonB^2)),digits=2)
  phase<-round(atan((-seasonB)/seasonA),digits=2)
  seasonality <- cbind(amplitude,phase)
  return(seasonality)
}