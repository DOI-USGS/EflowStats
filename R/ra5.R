#' Function to return the RA5 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA5; Number of day rises. Compute the number of days in which the flow is greater than the previous day. RA5 
#' is the number of positive gain days divided by the total number of days in the flow record (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra5 numeric containing RA5 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
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