#' Function to return the normalized root mean square error between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the normalized root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmsne normalized root mean square error value between the two timeseries
#' @export
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rmsne(timeseries1,timeseries2)
rmsne<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
  sqerror<-((timeseries1-timeseries2)/timeseries1)^2
  sumsqerr<-sum(sqerror)
  n<-length(timeseries1)
  rmsne<-sqrt(sumsqerr/n)
  } else {rmsne<-NA}
  return(rmsne)
}