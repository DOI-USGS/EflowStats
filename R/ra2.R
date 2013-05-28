#' Function to return the RA2 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the rise rate for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra2 numeric containing the variability of the rise rate for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ra2(qfiletempf)
ra2 <- function(qfiletempf) {
  meanra2 <- ra1(qfiletempf, pref = "mean")
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays > 
                             0)
  sddra2 <- sd(findrisevalues)
  ra2 <- (sddra2 * 100)/meanra2
  return(ra2)
}