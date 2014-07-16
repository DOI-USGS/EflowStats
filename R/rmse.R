#' Function to return the root mean square error between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmse root mean square error value between the two timeseries
#' @export
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rmse(timeseries1,timeseries2)
rmse<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
  sqerror<-(timeseries1-timeseries2)^2
  sumsqerr<-sum(sqerror)
  n<-length(timeseries1)
  rmse<-sqrt(sumsqerr/n)
  } else {rmse<-NA}
  return(rmse)
}