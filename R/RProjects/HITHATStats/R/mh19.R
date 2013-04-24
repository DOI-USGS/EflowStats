#' Function to return the MH19 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the skewness in log of annual maximum flows.
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh19 numeric value of the skewness of log of maximum annual flows for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' x<-read.csv(load_data)
#' mh19(x)
mh19 <- function(x) {
  annmax <- aggregate(x$discharge,list(x$wy_val),FUN=max,na.rm=TRUE)
  log_disch <- log10(annmax$x)
  sumq3 <- sum(log_disch^3)
  sumq2 <- sum(log_disch^2)
  sumq <- sum(log_disch)
  num_years <- length(unique(x$wy_val))
  qstd <- sd(log_disch)
  mh19 <- ((num_years*num_years*sumq3) - (3*num_years*sumq*sumq2) + (2*sumq*sumq*sumq))/(num_years*(num_years-1)*(num_years-2)*qstd*qstd*qstd)
  return(mh19)
}