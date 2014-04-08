#' Function to return the TH3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and a threshold value obtained 
#' using the peakdata and getPeakThresh functions and calculates 
#' TH3; Seasonal predictability of nonflooding. Computed as the maximum proportion of a 365-day year that the 
#' flow is less than the 1.67-year flood threshold and also occurs in all years. Accumulate nonflood days that 
#' span all years. TH3 is maximum length of those flood-free periods divided by 365 (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @return tl4 numeric containing TH3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' th3(qfiletempf, 1158)
th3 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  qfiletempf$diff <- (qfiletempf$discharge-lfcrit)
  jul_day_sum <- aggregate(qfiletempf$diff, list(qfiletempf$jul_val), max)
  maxdur <- rep(0,nrow(jul_day_sum))
  flag <- 0
  for (i in 1:365) {
    if (jul_day_sum$x[i]<=0) {
      flag <- flag+1
      maxdur[i]<-flag 
    } else {
      maxdur[i] <- 0
      flag <- 0
    }
  }
  th3 <- max(maxdur)/365
  return(th3)
}