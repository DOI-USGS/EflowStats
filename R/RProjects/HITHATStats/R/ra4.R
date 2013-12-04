#' Function to return the RA4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the fall rate for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra4 numeric containing the variability of the fall rate for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ra4(qfiletempf)
ra4 <- function(qfiletempf) {
  meanra4 <- ra3(qfiletempf, pref = "mean")
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findfallvalueneg <- subset(diffbtdays, diffbtdays < 
                               0)
  findfallvalues <- abs(findfallvalueneg)
  sddra4 <- sd(findfallvalues)
  ra4 <- (sddra4 * 100)/meanra4
  return(ra4)
}