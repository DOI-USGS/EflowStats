#' Function to return the ML18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability in base flow. Compute the standard deviation for the ratios of 7-day moving 
#' average flows to mean annual flows for each year. ML18 is the standard deviation time 100 divided by 
#' the mean of the ratios.
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ml18 numeric value of the variability in base flow for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ml18(qfiletempf)
ml18 <- function(qfiletempf) {
  bfibyyear <- bfi(qfiletempf)  
  sdbfi <- sd(bfibyyear)
  meanbfi <- mean(bfibyyear)
  ml18 <- (sdbfi/meanbfi)*100
  return(ml18)
}