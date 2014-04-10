#' Function to return the ML18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability in base flow index 1. Compute the standard deviation for the ratios of minimum 7-day 
#' moving average flows to mean annual flows for each year.  ML18 is the standard deviation times 100 divided by 
#' the mean of the ratios. (percent-spatial)
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ml18 numeric value of ML18 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml18(qfiletempf)
ml18 <- function(qfiletempf) {
  bfibyyear <- bfi(qfiletempf)  
  sdbfi <- sd(bfibyyear)
  meanbfi <- mean(bfibyyear)
  ml18 <- round((sdbfi/meanbfi),digits=2)*100
  return(ml18)
}