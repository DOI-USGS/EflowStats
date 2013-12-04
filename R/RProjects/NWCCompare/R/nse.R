#' Function to return the Nash-Sutcliffe value between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the Nash-Sutcliffe value
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return nse Nash-Sutcliffe value between the two timeseries
#' @export
#' @examples
#' obs_data<-paste(system.file(package="NWCCompare"),"/data/obs_data.csv",sep="")
#' mod_data<-paste(system.file(package="NWCCompare"),"/data/mod_data.csv",sep="")
#' timeseries1<-read.csv(obs_data)$discharge
#' timeseries2<-read.csv(mod_data)$discharge
#' nse(timeseries1,timeseries2)
nse<-function(timeseries1,timeseries2) {
  numerat<-sum((timeseries1-timeseries2)^2,na.rm=TRUE)
  denomin<-sum((timeseries1-mean(timeseries1,na.rm=TRUE))^2,na.rm=TRUE)  #6/18/11: NSE value calculation has been fixed
  nse<-(1-(numerat/denomin))
  return(nse)
}