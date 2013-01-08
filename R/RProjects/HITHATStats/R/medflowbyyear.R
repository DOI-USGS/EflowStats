#' Function to return the median flow by year for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the median flow by year for the entire record
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return medflowbyyear numeric value of the median flow by year for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' medflowbyyear(x)
medflowbyyear <- function(qfiletempf) {
  medflowbyyear<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                           median, na.rm=TRUE)
  return(medflowbyyear)
}