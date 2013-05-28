#' Function to return the DL13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of annual minimum 30-day moving average divided by the median for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl13 numeric containing the mean of annual minimum 30-day moving average divided by the median for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl13(qfiletempf)
dl13 <- function(qfiletempf) {
  meanmin <- dl4(qfiletempf)
  medianq <- median(qfiletempf$discharge)
  dl13 <- meanmin/medianq
  return(dl13)
}