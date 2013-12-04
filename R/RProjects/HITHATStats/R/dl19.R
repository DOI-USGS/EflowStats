#' Function to return the DL19 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability of the annual number of zero-flow days for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl19 numeric containing the variability of the annual number of zero-flow days for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl19(qfiletempf)
dl19 <- function(qfiletempf) {
  subset_zero <- subset(qfiletempf,qfiletempf$discharge==0)
  if (nrow(subset_zero)>0) {
    subset_zero$discharge <- subset_zero$discharge+1
    zero_cnts <- aggregate(subset_zero$discharge, list(qfiletempf$wy_val), sum)
  meanzero <- mean(zero_cnts$x)
  sdzero <- sd(zero_cnts$x)
  dl19 <- (100*sdzero)/meanzero
  } else {
    dl19 <- 'NA'
  }
  return(dl19)
}