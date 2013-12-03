#' This is a function to deseasonalize streamflow data in order to compute the 7 statistics of daily streamflow
#' used by Archfield et al., under revision (June 2013). Input to the function is a
#' time series of streamflow with date in the format Y-m-d. Data should be arranged in 
#' two columns:  1) date and 2) streamflow.
#' Created May 29, 2013 and functions are modified from previous versions of this code.
#' 
#' @param timeseries1 data frame of daily flow data
#' @return deseason data frame of deseasonalized streamflow
#' @export
#' @examples
#' load_data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
#' timeseries1<-read.csv(load_data,stringsAsFactors=FALSE)
#' timeseries1<-data.frame(timeseries1$date,timeseries1$discharge,timeseries1$month_val,stringsAsFactors=FALSE)
#' timeseries1$date<-as.Date(timeseries1$timeseries1.date,"%m/%d/%y")
#' timeseries1<-data.frame(timeseries1$date,timeseries1$timeseries1.discharge,timeseries1$timeseries1.month_val,stringsAsFactors=FALSE)
#' colnames(timeseries1)<-c("date","flow","month_val")
#' deseason(timeseries1)
deseason<-function(timeseries) {
  
  #Begin the deseasonalizng process by querying the time series for each month
  #and storing the monthly average  	
  monmeans<-aggregate(timeseries$flow, list(timeseries$month_val), FUN=mean, na.rm=TRUE)
  deseas<-merge(timeseries,monmeans,by.x="month_val",by.y="Group.1")
  
  deseason<-data.frame(as.Date(deseas$date,"%m/%d/%y"),deseas$flow-deseas$x)
  colnames(deseason)<-c("date","flow")
  return(deseason)
}