#' Function to return the MA36-MA40 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "month_val" and calculates 
#' MA36; variability across monthly flows. Compute the minimum, maximum, and mean flows for each month 
#' in the entire flow record.  MA36 is the maximum monthly flow minus the minimum monthly flow divided by the 
#' median monthly flow (dimensionless-spatial). 
#' MA37; Variability across monthly flows.  Compute the first (25th 
#' percentile) and the third (75th percentile) quartiles (every month in the flow record).  MA37 is the third 
#' quartile minus the first quartile divided by the median of the monthly means (dimensionless-spatial). 
#' MA38; Variability across monthly flows.  Compute the 10th and 90th percentiles for the monthly means (every month in 
#' the flow record).  MA38 is the 90th percentile minus the 10th percentile divided by the median of the monthly 
#' means (dimensionless-spatial). 
#' MA39; Variability across monthly flows.  Compute the standard deviation for the 
#' monthly means.  MA39 is the standard deviation times 100 divided by the mean of the monthly means (percent-spatial). 
#' MA40; Skewness in the monthly flows.  MA40 is the mean of the monthly flow means minus the median of the monthly 
#' means divided by the median of the monthly means (dimensionless-spatial).
#' 
#' @param qfiletemp data frame containing a "discharge" column containing daily flow values
#' @return ma36.40 list containing MA36-MA40 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma36.40(qfiletempf)
ma36.40 <- function(qfiletemp) {
  meanbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val,qfiletemp$year_val), FUN = mean, na.rm=TRUE)
  colnames(meanbymon) <- c("Month","Year","meanmo")
  maxbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val,qfiletemp$year_val), FUN = max, na.rm=TRUE)
  colnames(maxbymon) <- c("Month","Year","maxmo")
  minbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val,qfiletemp$year_val), FUN = min, na.rm=TRUE)
  colnames(minbymon) <- c("Month","Year","minmo")
  sortmeanbymon <- sort(meanbymon$meanmo)
  perc_10 <- quantile(sortmeanbymon,.1,type=6)
  perc_25 <- quantile(sortmeanbymon,.25,type=6)
  perc_75 <- quantile(sortmeanbymon,.75,type=6)
  perc_90 <- quantile(sortmeanbymon,.9,type=6)
  ma36 <- round((max(meanbymon$meanmo)-min(meanbymon$meanmo))/median(meanbymon$meanmo),digits=2)
  ma37 <- round((perc_75-perc_25)/median(meanbymon$meanmo),digits=2)
  ma38 <- round((perc_90-perc_10)/median(meanbymon$meanmo),digits=2)
  ma39 <- round((sd(meanbymon$meanmo)*100)/mean(meanbymon$meanmo),digits=2)
  ma40 <- round((mean(meanbymon$meanmo)-median(meanbymon$meanmo))/median(meanbymon$meanmo),digits=2)
  ma36.40 <- list(ma36,ma37,ma38,ma39,ma40)
  return(ma36.40)
}