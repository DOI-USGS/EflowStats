#' Function to return the MA3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean (or median - use preference option) of the coefficients of variation 
#' (standard deviation/mean) for each year. Compute the coefficent of variation for each year 
#' of daily flows. Compute the mean/median of the annual coefficients of variation (percent).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ma3 numeric value of MA3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma3(qfiletempf)
ma3 <- function(qfiletempf, pref = "mean") {
  sdbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                      FUN = sd, na.rm=TRUE)
  colnames(sdbyyr) <- c("Year", "sdq")
  meanbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                        mean, na.rm=TRUE)
  colnames(meanbyyr) <- c("Year", "meanq")
  dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
                         meanbyyr$meanq)
  colnames(dfcvbyyr) <- c("Year", "sdq", "meanq")
  cvbyyr <- dfcvbyyr$sdq/ma1(qfiletempf)
  dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr)
  colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", 
                           "cvq")
  if (pref == "median") {
    medcv <- round(median(dfcvbyyrf$cvq, na.rm=TRUE),digits=2)
    ma3 <- (medcv) * 100
  }
  else {
    meancv <- round(mean(dfcvbyyrf$cvq, na.rm=TRUE),digits=2)
    ma3 <- (meancv) * 100
  }
  return(ma3)
}