#' Function to return the drainage area of a site from the NWISWeb sites web service
#' 
#' This function accepts a url parameter that already contains the desired 
#' NWIS site. It returns a drain_area value.
#' 
#' @param drain_url string containing the url for the retrieval
#' @return drain_area a numeric value of the drainage area for a given site
#' @export
#' @examples
#' drainage_url<-"http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
#' sites<-"02177000"
#' drain_url<-paste(drainage_url,sites,sep="")
#' \dontrun{
#' da <- getDrainageArea(drain_url)
#' }
getDrainageArea <- function(drain_url){
  cat(paste("Retrieving data from: \n", drain_url, "\n", sep=" "))
  site_service<-read.delim(drain_url, header=TRUE, quote="\"", dec=".", sep="\t", colClasses=c("character"), fill=TRUE, comment.char="#")
  site_service<-site_service[2,]
  drain_area<-as.numeric(site_service$drain_area_va)
  return (drain_area)
}