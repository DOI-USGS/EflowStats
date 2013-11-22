#' Function to return a specified flood threshold using data from the NWISWeb peak values service
#' 
#' This function accepts obs_data and peak_data data frames that contain observed data and peak annual flow data for the desired 
#' NWIS site and a 'perc' percentile value. It returns a value 'thresh' calculated by: 1.67-year flood threshold (Olden and Poff, 2003)-For indices 
#' FH11, DH22, DH23, DH24, TA3, and TH3, compute the log10 of the peak annual flows. Compute the log10 of the daily 
#' flows for the peak annual flow days. Calculate the coefficients for a linear regression equation for logs of peak 
#' annual flow versus logs of average daily flow for peak days. Using the log peak flow for the 1.67-year recurrence 
#' interval (60th percentile) as input to the regression equation, predict the log10 of the average daily flow. The 
#' threshold is 10 to the log10 (average daily flow) power (cubic feet per second). for the 5-year recurrence interval 
#' (80th) percentile, used by indices TL3 and TL4, follow the same process, inputing a different 'perc' value.
#' 
#' @param obs_data data frame containing processed NWIS daily flow data for the site
#' @param peakValues data frame containing NWIS annual peak flow data for the site
#' @param perc value containing the desired percentile to be calculated
#' @return thresh numeric containing the 1.67-year flood threshold
#' @export
#' @examples
#' qfiletempf<-sampleData
#' sites<-"02178400"
#' peakValues<-getPeakData(sites)
#' getPeakThresh(qfiletempf,peakValues,.6)
getPeakThresh <- function(obs_data,peakValues,perc) {
  peakDaily <- obs_data[obs_data$date %in% peakValues$date,]
  peakDaily$logval <- log10(peakDaily$discharge)
  peakInst <- peakValues[as.numeric(peakValues$wy_val)>=min(as.numeric(peakDaily$wy_val)) & as.numeric(peakValues$wy_val)<=max(as.numeric(peakDaily$wy_val)),]
  dailyMean <- mean(peakDaily$logval)
  instMean <- mean(peakInst$logval)
  num <- sum((peakDaily$logval-dailyMean)*(peakInst$logval-instMean))
  denom <- sum((peakInst$logval-instMean)*(peakInst$logval-instMean))
  b <- num/denom
  a <- dailyMean - (b*instMean)
  isolateq <- peakInst$logval
  sortq <- sort(isolateq)
  lfcrit <- quantile(sortq,perc,type=6)
  lq167 <- a + (b*lfcrit)
  thresh <- 10^(lq167)
return(thresh)
}