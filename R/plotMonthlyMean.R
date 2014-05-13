#' Function to plot the monthly mean time series for a given daily data series
#' 
#' This function accepts a data frame containing daily data and a site number and returns a graph of mean monthly data
#' 
#' @param x data frame containing mean monthly values
#' @param station station number for plot title 
#' @export
#' @examples
#' qfiletempf<-sampleData
#' meanmonts<-monthlyMeanTs(qfiletempf)
#' plotMonthlyMean(meanmonts,'02178400')
plotMonthlyMean <- function(x,station){
  plot(x$datenum,x$Mean_disch,xlab="",ylab="Discharge(cfs)",col="blue",type="o",main=paste("Monthly Average Flow at station ",station,sep=""))
}
