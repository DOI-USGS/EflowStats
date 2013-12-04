#' Function to return the RA6 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the median positive change in log of discharge for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra6 numeric containing the median positive log of discharge change for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ra6(qfiletempf)
ra6 <- function(qfiletempf) {
  diffbtdays <- diff(log10(qfiletempf$discharge), lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays > 
                             0)
  ra6 <- median(findrisevalues)
  return(ra6)
}