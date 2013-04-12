#' Function to return the TA3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the seasonal predictability of floods above the 60th percentile for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ta3 numeric containing the max floods in a period over total floods for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' ta3(qfiletempf)
ta3 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.40))
  lfcrit <- sortq[frank]
  subset_crit <- subset(qfiletempf, qfiletempf$discharge>lfcrit)
  num_year <- aggregate(subset_crit$discharge, list(subset_crit$year_val,subset_crit$month_val), function(x) sum(!is.na(x)))
  names(num_year) <- c('year_val','month_val','num_days')
  ones_val<-rep(1,length(num_year$year_val))
  num_year$wy_val<-ifelse(as.numeric(num_year$month_val)>=10,as.character(as.numeric(num_year$year_val)+ones_val),num_year$year_val) 
  num_year$season <- ifelse(num_year$month_val==10,10,ifelse(num_year$month_val==11,10,ifelse(num_year$month_val==12,12,ifelse(num_year$month_val==1,12,ifelse(num_year$month_val==2,2,ifelse(num_year$month_val==3,2,ifelse(num_year$month_val==4,4,ifelse(num_year$month_val==5,4,ifelse(num_year$month_val==6,6,ifelse(num_year$month_val==7,6,ifelse(num_year$month_val==8,8,ifelse(num_year$month_val==9,8,99))))))))))))
  num_season <- aggregate(num_year$num_days, list(num_year$wy_val,num_year$season), sum)
  ta3 <- max(num_season$x)/sum(num_season$x)
  return(ta3)
}