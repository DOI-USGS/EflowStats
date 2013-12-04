#' Function to return the TL4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and a threshold value obtained 
#' using the peakdata and getPeakThresh functions and calculates 
#' TL4; Seasonal predictability of non-low flow. Compute the number of days that flow is above the 5-year flood 
#' threshold as the ratio of number of days to 365 or 366 (leap year) for each year. TL4 is the maximum of the yearly 
#' ratios (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh value containing the 5-year recurrence value for the site
#' @return tl4 numeric containing TL4 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' tl4(qfiletempf, 1161.38)
tl4 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  subset_crit <- subset(qfiletempf, qfiletempf$discharge>lfcrit)
  num_year <- aggregate(subset_crit$discharge, list(subset_crit$wy_val), function(x) sum(!is.na(x)))
  names(num_year) <- c('wy_val','num_days')
  num_year$ratio <- ifelse((as.numeric(num_year$wy_val)+1)/4==round((as.numeric(num_year$wy_val)+1)/4),num_year$num_days/366,num_year$num_days/365)
  tl4 <- max(num_year$ratio)
  return(tl4)
}