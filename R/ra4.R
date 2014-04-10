#' Function to return the RA4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA4; Variability in fall rate. Compute the standard deviation for the negative flow changes. RA4 is 100 times 
#' the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra4 numeric containing RA4 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra4(qfiletempf)
ra4 <- function(qfiletempf) {
  meanra4 <- ra3(qfiletempf, pref = "mean")
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findfallvalueneg <- subset(diffbtdays, diffbtdays < 
                               0)
  findfallvalues <- abs(findfallvalueneg)
  sddra4 <- sd(findfallvalues)
  ra4 <- round((sddra4 * 100)/meanra4,digits=2)
  return(ra4)
}