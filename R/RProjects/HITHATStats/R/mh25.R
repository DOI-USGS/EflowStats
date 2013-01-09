#' Function to return the MH25 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH25. High peak flow. Compute the average peak flow value for flow events above a 
#' threshold equal to three times the median for for the entire record. MH25 is the average peak flow divided by 
#' the median flow for the entire record.
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh25 numeric value of the mean daily flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' mh25(qfiletempf)
mh25 <- function(qfiletempf) {
  hfcrit <- 3 * ma2(qfiletempf)
  isolateq <- qfiletempf$discharge
  exchfcrit <- subset(isolateq, isolateq > hfcrit)
  meanex <- mean(exchfcrit)
  mh25 <- meanex/ma2(qfiletempf)
  return(mh25)
}