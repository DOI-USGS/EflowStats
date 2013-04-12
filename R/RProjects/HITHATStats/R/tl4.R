#' Function to return the TL4 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the predictability of flow above the 20th percentile for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return tl4 numeric containing the maximum of annual ratios of days above the 20th pctl to days in the year for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' tl4(qfiletempf)
tl4 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.80))
  lfcrit <- sortq[frank]
  subset_crit <- subset(qfiletempf, qfiletempf$discharge>lfcrit)
  ones_val <- rep(1, length(subset_crit$discharge))
  subset_crit$wy_val<-ifelse(as.numeric(subset_crit$month_val)>=10,subset_crit$year_val+ones_val,subset_crit$year_val) 
  num_year <- aggregate(subset_crit$discharge, list(subset_crit$wy_val), function(x) sum(!is.na(x)))
  names(num_year) <- c('wy_val','num_days')
  num_year$ratio <- ifelse((num_year$wy_val+1)/4==round((num_year$wy_val+1)/4),num_year$num_days/366,num_year$num_days/365)
  tl4 <- max(num_year$ratio)
  return(tl4)
}