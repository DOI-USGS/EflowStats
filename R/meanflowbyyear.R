#' Function to return the mean flow by year for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the mean flow by year for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return meanflowbyyear numeric value of the mean flow by year for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' meanflowbyyear(qfiletempf)
meanflowbyyear <- function(qfiletempf) {
  meanflowbyyear<-aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                            mean, na.rm=TRUE)
  colnames(meanflowbyyear) <- c("Year", "meanq")
  return(meanflowbyyear)
}