#' Function to return the ratio of the root mean square error to the standard deviation
#' 
#' This function accepts observed and modeled daily data series and returns the root mean square error/standard deviation
#' 
#' @param timeseries1 data frame containing value data for the observed timeseries
#' @param timeseries2 data frame containing value data for the modeled timeseries
#' @return rsr root mean square error/standard deviation for the two timeseries
#' @export
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rsr(timeseries1,timeseries2)
rsr<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
  sqerror<-(timeseries1-timeseries2)^2
  sumsqerr<-sum(sqerror)
  n<-length(timeseries1)
  rmse<-sqrt(sumsqerr/n)
  sdev <- sd(timeseries1,na.rm=TRUE)
  rsr <- rmse/sdev
  } else {rsr<-NA}
  return(rsr)
}