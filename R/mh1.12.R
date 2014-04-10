#' Function to return the MH1-MH12 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates the mean (or median-Use Preference option) maximum flows for each month across all years. Compute 
#' the maximum daily flow for each month over the entire flow record. For example, MH1 is the mean of the maximums 
#' of all January flow values over the entire record (cubic feet per second-temporal).
#' 
#' @param qfiletemp data frame containing a "discharge" column containing daily flow values
#' @return mh1.12 data frame containing the mean or median maximum flows for each month
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh1.12(qfiletempf)
mh1.12 <- function(qfiletemp) {
  maxbymonyr <- aggregate(qfiletemp$discharge, list(qfiletemp$year_val, qfiletemp$month_val), FUN = max, na.rm=TRUE)
  colnames(maxbymonyr) <- c("Year","Month","maxmo")
  meanmaxbymon <- aggregate(maxbymonyr$maxmo, list(maxbymonyr$Month), FUN = mean, na.rm=TRUE)
  colnames(meanmaxbymon) <- c("Month","meanmax")
  mh1 <- round(meanmaxbymon[1,2],digits=2)
  mh2 <- round(meanmaxbymon[2,2],digits=2)
  mh3 <- round(meanmaxbymon[3,2],digits=2)
  mh4 <- round(meanmaxbymon[4,2],digits=2)
  mh5 <- round(meanmaxbymon[5,2],digits=2)
  mh6 <- round(meanmaxbymon[6,2],digits=2)
  mh7 <- round(meanmaxbymon[7,2],digits=2)
  mh8 <- round(meanmaxbymon[8,2],digits=2)
  mh9 <- round(meanmaxbymon[9,2],digits=2)
  mh10 <- round(meanmaxbymon[10,2],digits=2)
  mh11 <- round(meanmaxbymon[11,2],digits=2)
  mh12 <- round(meanmaxbymon[12,2],digits=2)
  mh1.12 <- list(mh1,mh2,mh3,mh4,mh5,mh6,mh7,mh8,mh9,mh10,mh11,mh12)
  return(mh1.12)
}