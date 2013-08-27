#' Function to return the DL11 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL11; Annual minimum daily flow divided by the median for the entire record. Compute the minimum daily flow 
#' for each year. DL11 is the mean of these values divided by the median for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl11 numeric containing DL11 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl11(qfiletempf)
dl11 <- function(qfiletempf) {
  meanmin <- dl1(qfiletempf)
  medianq <- median(qfiletempf$discharge)
  dl11 <- meanmin/medianq
  return(dl11)
}