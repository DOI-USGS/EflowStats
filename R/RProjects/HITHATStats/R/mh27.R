#' Function to return the MH27 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH27. High peak flow. Compute the average peak flow value for flow events above a 
#' threshold equal to the 75th percentile value for for the entire record. MH27 is the average peak flow divided by 
#' the median flow for the entire record.
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh27 numeric value of the mean daily flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' mh27(qfiletempf)
mh27 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank75 <- floor(findrank(length(sortq),0.75))
  hfcrit75 <- sortq[frank75]
  exchfcrit <- subset(isolateq, isolateq > hfcrit75)
  meanex <- mean(exchfcrit)
  mh27 <- meanex/ma2(qfiletempf)
  return(mh27)
}