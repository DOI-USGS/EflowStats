#' Function to return the MA41.45 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and a value "drain_area" 
#' containing the drainage area (in square miles) and calculates 
#' MA41; Annual runoff. Compute the annual mean daily flows. MA41 is the mean of the annual means divided 
#' by the drainage area (cubic feet per second/square mile-temporal). 
#' MA42; Variability across annual flows.  MA42 is the maximum annual flow minus the minimum annual flow divided by 
#' the median of mean annual flows (dimensionless-spatial). 
#' MA43; Variability across annual flows.  Compute the first (25th percentile) and third (75th percentile) quartiles 
#' and the 10th and 90th percentiles for the annual means (every year in the flow record). MA43 is the third quartile 
#' minus the first quartile divided by the median of the annual means (dimensionless-spatial). 
#' MA44; Variability across annual flows.  Compute the first (25th percentile) and third (75th percentile) quartiles 
#' and the 10th and 90th percentiles for the annual means (every year in the flow record). MA44 is the 90th percentile 
#' minus the 10th percentile divided by the median of the annual means (dimensionless-spatial). 
#' MA45; Skewness in the annual flows. MA45 is the mean of the annual flow means minus the median of the annual means 
#' divided by the median of the annual means (dimensionless-spatial).
#' 
#' @param qfiletemp data frame containing a "discharge" column containing daily flow values
#' @param drain_area numeric containing the drainage area of the selected site
#' @return ma41.45 list of MA41-MA45 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletemp<-read.csv(load_data)
#' drain_area<-56.5
#' ma41.45(qfiletemp,drain_area)
ma41.45 <- function(qfiletemp,drain_area) {
  meanbyyr <- aggregate(qfiletemp$discharge, list(qfiletemp$wy_val), FUN = mean, na.rm=TRUE)
  colnames(meanbyyr) <- c("Year","meanyr")
  sortmeanbyyr <- sort(meanbyyr$meanyr)
  perc_10 <- sortmeanbyyr[floor(findrank(length(sortmeanbyyr), 0.1))]
  perc_25 <- sortmeanbyyr[floor(findrank(length(sortmeanbyyr), 0.25))]
  perc_75 <- sortmeanbyyr[floor(findrank(length(sortmeanbyyr), 0.75))]
  perc_90 <- sortmeanbyyr[ifelse(floor(findrank(length(sortmeanbyyr), 0.9))==0,1,0)]
  ma41 <- mean(meanbyyr$meanyr)/drain_area
  ma42 <- (max(meanbyyr$meanyr)-min(meanbyyr$meanyr))/median(meanbyyr$meanyr)
  ma43 <- (perc_25-perc_75)/median(meanbyyr$meanyr)
  ma44 <- (perc_10-perc_90)/median(meanbyyr$meanyr)
  ma45 <- (mean(meanbyyr$meanyr)-median(meanbyyr$meanyr))/median(meanbyyr$meanyr)
  ma41.45 <- list(ma41,ma42,ma43,ma44,ma45)
  return(ma41.45)
}