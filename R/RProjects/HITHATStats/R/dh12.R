#' Function to return the DH12 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the annual 7-day moving average maximum flow divided by the median flow of the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh12 numeric containing the annual 7-day moving average maximum flow divided by the median flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh12(qfiletempf)
dh12 <- function(qfiletempf) {
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  meanmax7day <- dh3(qfiletempf)
  dh12 <- meanmax7day/medianflow
  return(dh12)
}