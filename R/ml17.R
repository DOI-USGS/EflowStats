#' Function to return the ML17 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the base flow. Compute the mean annual flows. Compute the minimum of a 7-day moving average flow 
#' for each year and divide them by the mean annual flow for that year. ML17 is the mean (or median-Use 
#' Preference option) of those ratios (dimensionless-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ml17 numeric value of ML17 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml17(qfiletempf)
ml17 <- function(qfiletempf, pref = "mean") {
  bfibyyear <- bfi(qfiletempf)
  if (pref == "median") {
    ml17 <- median(bfibyyear)
  }
  else {
    ml17 <- mean(bfibyyear)
  }
  return(ml17)
}