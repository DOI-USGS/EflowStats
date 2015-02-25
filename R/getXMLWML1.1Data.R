#' Function to return data from the NWISWeb WaterML1.1 daily values service
#' 
#' This function accepts a url parameter that already contains the desired 
#' NWIS site, parameter code, statistic, startdate and enddate. It returns a 
#' data frame containing "Date" and "Discharge"
#' 
#' @param obs_url string containing the url for the retrieval
#' @return Daily a data frame containing columns 'Date' and 'Discharge'
#' @export
#' @import dataRetrieval
#' @examples
#' url<-"http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
#' sites<-"02177000"
#' startdate<-"2012-09-01"
#' enddate<-"2012-10-01"
#' offering<-'00003'
#' property<-'00060'
#' obs_url<-paste0(url,sites,'&startDT=',startdate,'&endDT=',enddate,
#'                '&statCd=',offering,'&parameterCd=',property)
#' \dontrun{
#' data <- getXMLWML1.1Data(obs_url)
#' }
getXMLWML1.1Data <- function(obs_url){
  cat(paste("Retrieving data from: \n", obs_url, "\n", sep = " "))
  Daily <- dataRetrieval::importWaterML1(obs_url, asDateTime = FALSE)
  Daily <- dataRetrieval::renameNWISColumns(Daily)
  Daily$dateTime <- as.Date(Daily$dateTime)
  Daily <- Daily[,c("dateTime","Flow")]
  colnames(Daily)<-c('date','discharge')
  return(Daily)
}
