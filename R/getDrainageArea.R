#' Function to return the drainage area of a site from the NWISWeb sites web service
#' 
#' This function accepts a url parameter that already contains the desired 
#' NWIS site. It returns a drain_area value.
#' 
#' @param site string containing the USGS site id
#' @return drain_area a numeric value of the drainage area for a given site
#' @export
#' @import dataRetrieval
#' @examples
#' sites<-"02177000"
#' \dontrun{
#' da <- getDrainageArea(sites)
#' }
getDrainageArea <- function(site){
  
  cat(paste("Retrieving data from USGS site: \n", site, "\n", sep=" "))
  siteFile <- dataRetrieval::readNWISsite(site)
  drain_area<-as.numeric(siteFile$drain_area_va)
  return (drain_area)
}
