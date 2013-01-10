#' Function to return the DL20 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the number of zero-flow months for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl20 numeric containing the number of zero-flow months for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl20(qfiletempf)
dl20 <- function(qfiletempf) {
  sumbymonyr <- aggregate(qfiletempf$discharge,list(qfiletempf$month_val,qfiletempf$year_val),FUN=sum,na.rm=TRUE)
  if (min(sumbymonyr$x)==0) {
    zeromon <- subset(sumbymonyr$x,sumbymonyr$x==0)
    dl20 <- nrow(zeromon)
  } 
  else {
    dl20 <- 'NA'
  }
  return(dl20)
}