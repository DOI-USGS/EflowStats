#' Function to return the monthly mean time series for a given daily data series
#' 
#' This function accepts a data frame containing daily data and returns a data frame containing mean monthly data
#' 
#' @param qfiletempf data frame containing daily value data 
#' @return meanmonts data frame containing mean monthly values
#' @export
#' @examples
#' qfiletempf<-sampleData
#' monthly.mean.ts(qfiletempf)
monthly.mean.ts <- function(qfiletempf) {
  meanmonts <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanmonts) <- c("Year","Month","Mean_disch")
  monthly_mean$datenum <- as.numeric(monthly_mean$Year)+as.numeric(monthly_mean$Month)*.091
  monthly_mean<-monthly_mean[order(monthly_mean$datetime),]
  return(meanmonts)
}