#' Function to return the MA41.45 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates MA41; annual runoff. Compute the annual mean daily flows. MA41 is the mean of the annual 
#' means divided by the drainage area. MA42; Variability across annual flows. MA42 is the maximum annual 
#' flow minus the minimum annual flow divided by the median annual flow. MA43; Same as MA42, but using the 
#' 25th and 75th percentiles. MA44; Same as MA42, but using the 10th and 90th percentiles. MA45; 
#' Skewness in the annual flows. MA45 is the mean of the annual flow means minus the median of the annual 
#' flows divided by the median of the annual means.
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
  meanbyyr <- aggregate(qfiletemp$discharge, list(qfiletemp$year_val), FUN = mean, na.rm=TRUE)
  colnames(meanbyyr) <- c("Year","meanyr")
  sortmeanbyyr <- sort(meanbyyr$meanyr)
  perc_10 <- floor(findrank(length(sortmeanbyyr), 0.1))
  perc_25 <- floor(findrank(length(sortmeanbyyr), 0.25))
  perc_75 <- floor(findrank(length(sortmeanbyyr), 0.75))
  perc_90 <- floor(findrank(length(sortmeanbyyr), 0.9))
  ma41 <- mean(meanbyyr$meanyr)/drain_area
  ma42 <- (max(meanbyyr$meanyr)-min(meanbyyr$meanyr))/median(meanbyyr$meanyr)
  ma43 <- (perc_75-perc_25)/median(meanbyyr$meanyr)
  ma44 <- (perc_90-perc_10)/median(meanbyyr$meanyr)
  ma45 <- (mean(meanbyyr$meanyr)-median(meanbyyr$meanyr))/median(meanbyyr$meanyr)
  ma41.45 <- list(ma41,ma42,ma43,ma44,ma45)
  return(ma41.45)
}