#' Function to return the MA12-23 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "month_val" and 
#' calculates the means (or medians - use preference option) of monthly flow values. Compute the means 
#' for each month over the entire record. For examples, MA12 is the mean of all January flow values.
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ma12.23 data frame containing the mean or medians for each month
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ma12.23(qfiletempf)
#' ma12.23(qfiletempf,pref="median")
ma12.23 <- function(qfiletempf, pref = "mean") {
  if (pref == "median") {
    medmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
                        median, na.rm=TRUE)
    ma12.23 <- data.frame(medmon)
  }
  else {
    meanmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
                         mean, na.rm=TRUE)
    ma12.23 <- data.frame(meanmon)
  }
  return(ma12.23)
}