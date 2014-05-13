#' Function to return the monthly mean time series for a given daily data series
#' 
#' This function accepts a data frame containing daily data and returns a data frame containing mean monthly data
#' 
#' @param qfiletempf data frame containing daily value data 
#' @return meanmonts data frame containing mean monthly values
#' @export
#' @examples
#' qfiletempf<-sampleData
#' monthlyMeanTs(qfiletempf)
monthlyMeanTs <- function(qfiletempf) {
  meanmonts <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanmonts) <- c("Year","Month","Mean_disch")
  meanmonts$datenum <- as.numeric(meanmonts$Year)+as.numeric(meanmonts$Month)*(1/12)
  meanmonts<-meanmonts[order(meanmonts$datenum),]
  return(meanmonts)
}