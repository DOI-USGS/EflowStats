#' Function to return all flow statistics for a set of USGS sites and start and end date
#' 
#' This function accepts a list of sites, start and end date and list of desired statistic groups and returns a data frame containing requested statistics
#' 
#' @param sites list of USGS station ids
#' @param startWY Character string of beginning water year, start date will be translated to 10/01
#' @param startWY Character string of beginning water year, end date will be translated to 10/01
#' @param stats string containing desired groups of statistics 
#' @return statsout data frame containing requested statistics for each station
#' @export
#' @examples
#' sites <- c("02177000", "02178400")
#' startWY <- "2009"
#' endWY <- "2013"
#' stats <- "magnifSeven,magStat,flowStat,durStat,timStat,rateStat"
#' \dontrun{
#' usgsStats <- ObservedStatsUSGS(sites,startdate,enddate,stats)
#' }
ObservedStatsUSGS <- function(sites,startWY,endWY,stats) {
  startWY <- paste(startWY,"10","01",sep="-")
  endWY <- paste(endWY,"09","30",sep="-")
  
  x_obs <- dataRetrieval::readNWISdv(sites, "00060", startWY, endWY)
  x_obs <- dataRetrieval::renameNWISColumns(x_obs)
  
  drain_area<-getDrainageArea(sites)
  
  x_obs <- dplyr::left_join(x_obs,drain_area,by="site_no")
  
  
  x_obs <- dplyr::group_by(x_obs,"site_no")

}
