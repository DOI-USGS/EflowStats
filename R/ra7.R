#' Function to return the RA7 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the median change in log of discharge for days with negative change for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra7 numeric containing the median log of discharge change for days with negative change for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ra7(qfiletempf)
ra7 <- function(qfiletempf) {
  disch_log10 <- ifelse(qfiletempf$discharge>0,log10(qfiletempf$discharge),log10(.01))
  diffbtdays <- diff(disch_log10, lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays < 
                             0)
  ra7 <- median(abs(fall_data_sub))
  return(ra7)
}