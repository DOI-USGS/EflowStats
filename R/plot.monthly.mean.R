#' Function to plot the monthly mean time series for a given daily data series
#' 
#' This function accepts a data frame containing daily data and a site number and returns a graph of mean monthly data
#' 
#' @param meanmonts data frame containing mean monthly values
#' @param station station number for plot title 
#' @export
#' @examples
#' qfiletempf<-sampleData
#' meanmonts<-monthly.mean.ts(qfiletempf)
#' plot.monthly.mean(meanmonts,'02178400')
plot.monthly.mean <- function(meanmonts,station) {
  plot(meanmonts$datenum,meanmonts$Mean_disch,xlab="",ylab="Discharge(cfs)",col="blue",type="o",main=paste("Monthly Average Flow at USGS station ",station,sep=""))
}
