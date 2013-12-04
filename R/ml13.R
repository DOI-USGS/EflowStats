#' Function to return the ML13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge","month_val" and "year_val" and 
#' calculates the Variability (coefficient of variation) across minimum monthly flow values. Compute the mean 
#' and standard deviation for the minimum monthly flows over the entire flow record. ML13 is the standard deviation 
#' times 100 divided by the mean minimum monthly flow for all years (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ml13 numeric value of ML13 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml13(qfiletempf)
ml13 <- function(qfiletempf) {
  minmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                     qfiletempf$month_val), FUN = min, na.rm=TRUE)
  colnames(minmonbyyr) <- c("Year", "Month", "minmo")
  sdminmonflows <- sd(minmonbyyr$minmo)
  meanminmonflows <- mean(minmonbyyr$minmo)
  ml13 <- (sdminmonflows * 100)/meanminmonflows
  return(ml13)
}