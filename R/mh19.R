#' Function to return the MH19 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the skewness in annual maximum flows (dimensionless-spatial). Use the equation:
#' MH19   numerator =   N2 ? sum(qm3)-3N ? sum(qm) ? sum(qm2) + 2 ? (sum(qm))3  
#' denominator = N ? (N-1) ? (N-2) ? stddev3  
#' Where:  N = Number of years
#' qm = Log10 (annual maximum flows)
#' stddev = Standard deviation of the annual maximum flows
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh19 numeric value of MH19 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh19(qfiletempf)
mh19 <- function(x) {
  annmax <- aggregate(x$discharge,list(x$wy_val),FUN=max,na.rm=TRUE)
  log_disch <- log10(annmax$x)
  sumq3 <- sum(log_disch^3)
  sumq2 <- sum(log_disch^2)
  sumq <- sum(log_disch)
  num_years <- length(unique(x$wy_val))
  qstd <- sd(log_disch)
  mh19 <- round(((num_years*num_years*sumq3) - (3*num_years*sumq*sumq2) + (2*sumq*sumq*sumq))/(num_years*(num_years-1)*(num_years-2)*qstd*qstd*qstd),digits=2)
  return(mh19)
}