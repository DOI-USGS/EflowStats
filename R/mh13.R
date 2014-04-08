#' Function to return the MH13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates MH13, variability (coefficient of variation) across maximum monthly flow values. Compute the mean 
#' and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation 
#' times 100 divided by the mean maximum monthly flow for all years (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh13 numeric value of MH13 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh13(qfiletempf)
mh13 <- function(qfiletempf) {
  maxmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                     qfiletempf$month_val), FUN = max, na.rm=TRUE)
  colnames(maxmonbyyr) <- c("Year", "Month", "maxmo")
  sdmaxmonflows <- sd(maxmonbyyr$maxmo)
  meanmaxmonflows <- mean(maxmonbyyr$maxmo)
  mh13 <- (sdmaxmonflows * 100)/meanmaxmonflows
  return(mh13)
}