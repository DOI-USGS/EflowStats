#' Function to return the DH14 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH14; Flood duration. Compute the mean of the mean monthly flow values. Find the 95th percentile for the 
#' mean monthly flows. DH14 is the 95th percentile value divided by the mean of the monthly means 
#' (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh14 numeric containing DH14 for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dh14(qfiletempf)
dh14 <- function(qfiletempf) {
  meanmonthly <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val,qfiletempf$wy_val), mean, na.rm=TRUE)
  meanflow <- mean(meanmonthly$x)
  isolateq <- meanmonthly$x
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.05))
  hfcrit <- sortq[frank]
  dh14 <- hfcrit/meanflow
  return(dh14)
}