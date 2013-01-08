#' Function to return the MA36-MA40 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "month_val" and 
#' calculates MA36; variability across monthly flows. Compute the minimum, maximum and mean flows for 
#' each month in the entire flow record. MA36 is the maximum monthly flow minus the minimum monthly flow 
#' divided by the median monthly flow. MA37; Same as MA37, but for the 25th and 75th percentiles. MA38; 
#' Same as MA37, but for the 10th and 90th percentiles. MA39; Variability across monthly flows. Compute the 
#' standard deviation for the monthly means. MA39 is the standard deviation times 100 divided by the mean 
#' of the monthly means. MA40; Skewness in the monthly flows. MA40 is the mean of the monthly flow means 
#' minus the median of the monthly means divided by the median of the monthly means.
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ma36.40 list containing MA36-MA40 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' ma36.40(x)
ma36.40 <- function(qfiletemp) {
  meanbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanbymon) <- c("Month","meanmo")
  maxbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val), FUN = max, na.rm=TRUE)
  colnames(maxbymon) <- c("Month","maxmo")
  minbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val), FUN = min, na.rm=TRUE)
  colnames(minbymon) <- c("Month","minmo")
  sortmeanbymon <- sort(meanbymon$meanmo)
  perc_10 <- floor(findrank(length(sortmeanbymon), 0.1))
  perc_25 <- floor(findrank(length(sortmeanbymon), 0.25))
  perc_75 <- floor(findrank(length(sortmeanbymon), 0.75))
  perc_90 <- floor(findrank(length(sortmeanbymon), 0.9))
  ma36 <- (max(maxbymon$maxmo)-min(minbymon$minmo))/median(meanbymon$meanmo)
  ma37 <- (perc_75-perc_25)/median(meanbymon$meanmo)
  ma38 <- (perc_90-perc_10)/median(meanbymon$meanmo)
  ma39 <- (sd(meanbymon$meanmo)*100)/mean(meanbymon$meanmo)
  ma40 <- (mean(meanbymon$meanmo)-median(meanbymon$meanmo))/median(meanbymon$meanmo)
  ma36.40 <- list(ma36,ma37,ma38,ma39,ma40)
  return(ma36.40)
}