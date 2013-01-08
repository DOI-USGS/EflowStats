#' Function to return the MH26 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH26. High peak flow. Compute the average peak flow value for flow events above a 
#' threshold equal to seven times the median for for the entire record. MH26 is the average peak flow divided by 
#' the median flow for the entire record.
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh26 numeric value of the mean daily flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' mh26(x)
mh26 <- function(qfiletempf) {
  hfcrit <- 7 * ma2(qfiletempf)
  isolateq <- qfiletempf$discharge
  exchfcrit <- subset(isolateq, isolateq > hfcrit)
  meanex <- mean(exchfcrit)
  mh26 <- meanex/ma2(qfiletempf)
  return(mh26)
}