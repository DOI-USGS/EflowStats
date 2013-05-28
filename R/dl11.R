#' Function to return the DL11 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of annual minimum flows divided by the median for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl11 numeric containing the mean of annual minimum flows divided by the median for the given data frame
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