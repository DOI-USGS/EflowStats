#' Function to return the MH15-MH17 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flow discharge indexes. 
#' MH15; High flow discharge index. Compute the 1-percent exceedence value for the entire data record. MH15 is 
#' the 1-percent exceedence value divided by the median flow for the entire record (dimensionless-spatial). 
#' MH16; Compute the 10-percent exceedence value for the entire data record. MH16 is the 10-percent exceedence 
#' value divided by the median flow for the entire record (dimensionless-spatial). 
#' MH17; Compute the 25-percent exceedence value for the entire data record. MH17 is the 25-percent exceedence 
#' value divided by the median flow for the entire record (dimensionless-spatial). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh15.17 list of numeric value of MH15, MH16 and MH17 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh15.17(qfiletempf)
mh15.17 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  hfcrit10 <- quantile(sortq,.9,type=6)
  hfcrit1 <- quantile(sortq,.99,type=6)
  hfcrit25 <- quantile(sortq,.75,type=6)
  mh15 <- round(hfcrit1/ma2(qfiletempf),digits=2)
  mh16 <- round(hfcrit10/ma2(qfiletempf),digits=2)
  mh17 <- round(hfcrit25/ma2(qfiletempf),digits=2)
  mh15.17 <- list(mh15,mh16,mh17)
  return(mh15.17)
}