#' Function to return the MH15-MH17 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH15. High flow discharge index. compute the 1% exceedence value for the entire data record. 
#' MH15 is the 1% exceedence value divided by the median flow for the entire record. MH16 is the 10% 
#' exceedence value divided by the median flow for the entire record. MH17 is the 25% exceedence value 
#' divided by the median flow for the entire record. 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh15.17 list of numeric value of the 1%, 10% and 25% exceedence values divided by median flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' mh15.17(qfiletempf)
mh15.17 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank10 <- floor(findrank(length(sortq), 0.1))
  frank1 <- floor(findrank(length(sortq),0.01))
  frank25 <- floor(findrank(length(sortq),0.25))
  hfcrit10 <- sortq[frank10]
  hfcrit1 <- sortq[frank1]
  hfcrit25 <- sortq[frank25]
  mh15 <- hfcrit1/ma2(qfiletempf)
  mh16 <- hfcrit10/ma2(qfiletempf)
  mh17 <- hfcrit25/ma2(qfiletempf)
  mh15.17 <- list(mh15,mh16,mh17)
  return(mh15.17)
}