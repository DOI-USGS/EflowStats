#' Function to return NWC Intercomparison portal sites with modeled data for a given model
#' 
#' This function accepts a url and returns a list of sites with modeled data
#' 
#' @param scenario_url url for SOS service for desired model data
#' @return getcap list of sites with data for the chosen model
#' @export
#' @examples
#' scenario_url <- "http://cida.usgs.gov/nwc/thredds/sos/watersmart/stats/stats-SE-DENSE1-2.03.nc?request=GetCapabilities&service=SOS&version=1.0.0"
#' getScenarioSites(scenario_url)
getScenarioSites <- function(scenario_url){
  cat(paste("Retrieving list of sites in scenario from: \n", scenario_url, "\n", sep=" "))
  doc<-xmlTreeParse(scenario_url, getDTD=F, useInternalNodes=TRUE)
  sites<-xpathSApply(doc, "//@gml:id")
  scenario_sites<-vector(length=length(sites))
  scenario_sites<-unname(sites)
  modprop<-xpathSApply(doc, "//*[local-name() = 'observedProperty']/@xlink:href")[["href"]]
  getcap<-list(scenario_sites=scenario_sites,modprop=modprop)
  return (getcap)
}