#' Function to compute the AR(1) correlation coefficient for a given data series
#' 
#' This function accepts a data frame containing daily streamflow data and returns the AR(1) 
#' correlation coefficient
#' 
#' @param data data frame containing daily discharge data
#' @return ar1 AR(1) correlation coefficient
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ar1(qfiletempf)
ar1 <- function(data) {
  #First, deseasonalize the time series using the long-term monthly means
  ds.timeseries<-deseason(timeseries)  
  #Fit AR(1) model to deseasonalized data but first standardize deseasonlized time series
  ds_std_flows<-scale(ds.timeseries$flow, center = TRUE, scale = TRUE)
  armdl<-ar(ds_std_flows, aic = FALSE, order.max = 1, method="yule-walker")
  ar1<-round(armdl$ar,digits=2)
  return(ar1)
}