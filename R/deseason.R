#' This is a function to deseasonalize streamflow data in order to compute the 7 statistics of daily streamflow
#' used by Archfield et al., under revision (June 2013). Input to the function is a
#' time series of streamflow with date in the format Y-m-d. Data should include date, month_val and discharge 
#' 
#' @param timeseries data frame of daily flow data
#' @return deseason data frame of deseasonalized streamflow
#' @export
#' @examples
#' timeseries1<-sampleData
#' deseason1 <- deseason(timeseries1)
deseason<-function(timeseries) {
  
  #Begin the deseasonalizng process by querying the time series for each month
  #and storing the monthly average  	
  monmeans<-aggregate(timeseries$discharge, list(timeseries$month_val), FUN=mean, na.rm=TRUE)
  deseas<-merge(timeseries,monmeans,by.x="month_val",by.y="Group.1")
  
  deseasonv<-data.frame(deseas$date,deseas$discharge-deseas$x,stringsAsFactors=FALSE)
  colnames(deseasonv)<-c("date","discharge")
  return(deseasonv)
}