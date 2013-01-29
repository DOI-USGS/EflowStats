#' Function to return the root mean square error between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmse root mean square error value between the two timeseries
#' @export
#' @examples
#' obs_data<-paste(system.file(package="NWCCompare"),"/data/obs_data.csv",sep="")
#' mod_data<-paste(system.file(package="NWCCompare"),"/data/mod_data.csv",sep="")
#' timeseries1<-read.csv(obs_data)$discharge
#' timeseries2<-read.csv(mod_data)$discharge
#' rmse(timeseries1,timeseries2)
rmse<-function(timeseries1,timeseries2) {
  sqerror<-(timeseries1-timeseries2)^2
  sumsqerr<-sum(sqerror)
  n<-length(timeseries1)
  rmse<-sqrt(sumsqerr/n)
  return(rmse)
}