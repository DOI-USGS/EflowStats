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
#' getPeakData(sites)
getPeakData <- function(sites){
urlpeak <- "http://nwis.waterdata.usgs.gov/nwis/peak?site_no"
url <- paste(urlpeak,"=",sites,"&agency_cd=USGS&format=rdb",sep="")
doc<-read.table(url,sep="\t",stringsAsFactors=FALSE)
peakValues<-data.frame(as.Date(doc$V3[3:nrow(doc)],format="%Y-%m-%d"),as.numeric(doc$V5[3:nrow(doc)]))
colnames(peakValues)<-c('date','discharge')
peakValues$year_val<-substr(peakValues$date,1,4)
peakValues$month_val<-substr(peakValues$date,6,7)
peakValues$wy_val<-ifelse(as.numeric(peakValues$month_val)>=10,as.character(as.numeric(peakValues$year_val)+1),peakValues$year_val) 
peakValues$logval <- log10(peakValues$discharge)
return (peakValues)
}