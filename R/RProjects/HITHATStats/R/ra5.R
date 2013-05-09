#' Function to return the RA5 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the ratio of gain days to total number of days for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra5 numeric containing the ratio of gain days to total days for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ra5(qfiletempf)
ra5 <- function(qfiletempf) {
  sub_length <- nrow(qfiletempf)-1
  counter <- 0
  for (i in 1:sub_length) {
    if (qfiletempf$discharge[i+1] - qfiletempf$discharge[i] > 0) { 
      counter <- counter+1
    }
  }
  ra5 <- counter/(sub_length+1)
  return(ra5)
}