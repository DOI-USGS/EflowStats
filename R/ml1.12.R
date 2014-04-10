#' Function to return the ML1-ML12 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates the Mean (or median-Use Preference option) minimum flows for each month across all years. Compute 
#' the minimum daily flow for each month over the entire flow record. For example, ML1 is the mean of the minimums 
#' of all January flow values over the entire record (cubic feet per second-temporal).
#' 
#' @param qfiletemp data frame containing a "discharge" column containing daily flow values
#' @return ml1.12 data frame containing the mean or median minimum flows for each month
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml1.12(qfiletempf)
ml1.12 <- function(qfiletemp) {
  minbymonyr <- aggregate(qfiletemp$discharge, list(qfiletemp$year_val, qfiletemp$month_val), FUN = min, na.rm=TRUE)
  colnames(minbymonyr) <- c("Year","Month","minmo")
  meanminbymon <- aggregate(minbymonyr$minmo, list(minbymonyr$Month), FUN = mean, na.rm=TRUE)
  colnames(meanminbymon) <- c("Month","meanmin")
  ml1 <- round(meanminbymon[1,2],digits=2)
  ml2 <- round(meanminbymon[2,2],digits=2)
  ml3 <- round(meanminbymon[3,2],digits=2)
  ml4 <- round(meanminbymon[4,2],digits=2)
  ml5 <- round(meanminbymon[5,2],digits=2)
  ml6 <- round(meanminbymon[6,2],digits=2)
  ml7 <- round(meanminbymon[7,2],digits=2)
  ml8 <- round(meanminbymon[8,2],digits=2)
  ml9 <- round(meanminbymon[9,2],digits=2)
  ml10 <- round(meanminbymon[10,2],digits=2)
  ml11 <- round(meanminbymon[11,2],digits=2)
  ml12 <- round(meanminbymon[12,2],digits=2)
  ml1.12 <- list(ml1,ml2,ml3,ml4,ml5,ml6,ml7,ml8,ml9,ml10,ml11,ml12)
  return(ml1.12)
}