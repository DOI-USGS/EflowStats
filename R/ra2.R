#' Function to return the RA2 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA2; Variability in rise rate. Compute the standard deviation for the positive flow changes. RA2 is 100 times 
#' the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra2 numeric containing RA2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra2(qfiletempf)
ra2 <- function(qfiletempf) {
  meanra2 <- ra1(qfiletempf, pref = "mean")
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays > 
                             0)
  sddra2 <- sd(findrisevalues)
  ra2 <- round((sddra2 * 100)/meanra2,digits=2)
  return(ra2)
}