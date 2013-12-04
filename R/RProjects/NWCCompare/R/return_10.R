#' Function to return the 10 year return value for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the 10 year return value
#' 
#' @param qfiletempf data frame containing value data for the chosen timeseries
#' @return return_10 10-year return value for the given data frame
#' @export
#' @examples
#' data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
#' qfiletempf<-read.csv(data,stringsAsFactors=FALSE)
#' return_10(qfiletempf)
return_10 <- function(qfiletempf) {
  annual_max <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), max, na.rm=TRUE)
  sort_annual_max <- sort(annual_max$x)
  rank_10 <- floor(findrank(length(sort_annual_max),0.10))
  return_10 <- sort_annual_max[rank_10]
  return(return_10)
}