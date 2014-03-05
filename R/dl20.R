#' Function to return the DL20 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL20; Number of zero-flow months. While computing the mean monthly flow values, count the number of months 
#' in which there was no flow over the entire flow record (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl20 numeric containing DL20 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl20(qfiletempf)
dl20 <- function(qfiletempf) {
  sumbymonyr <- aggregate(qfiletempf$discharge,list(qfiletempf$month_val,qfiletempf$year_val),FUN=sum,na.rm=TRUE)
  if (min(sumbymonyr$x)==0) {
    zeromon <- subset(sumbymonyr$x,sumbymonyr$x==0)
    dl20 <- length(zeromon)
  } 
  else {
    dl20 <- 0
  }
  return(dl20)
}