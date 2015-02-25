#' Function to return data from the NWISWeb peak values service
#' 
#' This function accepts a sites parameter that contains the desired NWIS site. It returns a data frame 
#' containing peak instantaneous annual flow data for the site. 
#' 
#' @param sites string containing the site for the retrieval
#' @return peakValues a data frame containing columns 'Date' and 'Discharge'
#' @export
#' @examples
#' sites<-"02178400"
#' \dontrun{
#' peakData <- getPeakData(sites)
#' }
getPeakData <- function(sites){
  
  peakValues <- dataRetrieval::readNWISpeak(sites, "", "")
  peakValues <- peakValues[,c("peak_dt","peak_va")]
  colnames(peakValues)<-c('date','discharge')
  
  peakValues$year_val<-substr(peakValues$date,1,4)
  peakValues$month_val<-substr(peakValues$date,6,7)
  peakValues$wy_val<-as.numeric(ifelse(as.numeric(peakValues$month_val)>=10,as.character(as.numeric(peakValues$year_val)+1),peakValues$year_val)) 
  peakValues$logval <- log10(peakValues$discharge)
  peakValues <- peakValues[,c(1:2,5:6)]
  return (peakValues)
}