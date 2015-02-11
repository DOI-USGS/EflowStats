#' Function to return flow data for a of USGS site and start and end date
#' 
#' This function accepts a list of sites, start and end date and returns a data frame containing requested flow data
#' 
#' @param sites USGS station ids
#' @param startdate beginning water year, will be translated to 10/01
#' @param enddate ending water year, will be translated to 09/30
#' @return dataOut data frame containing requested flow data for the stations
#' @export
#' @examples
#' sites <- c("02177000", "02178400")
#' startdate <- "2009"
#' enddate <- "2013"
#' data <- getDataUSGS(sites,startdate,enddate)
getDataUSGS <- function(sites,startdate,enddate) {
  startdate <- paste(startdate,"10","01",sep="-")
  enddate <- paste(enddate,"09","30",sep="-")

  dataOut <- list()
  for (i in 1:length(sites)) {
    site=sites[i]

    x_obs <- dataRetrieval::readNWISdv(site, "00060", startdate, enddate) 
    x_obs <- renameNWISColumns(x_obs)
    x_obs <- x_obs[,c("Date","Flow")]
    names(x_obs) <- c("date","discharge")
    if (nrow(x_obs)>2) {
      obs_data <- get_obsdata(x_obs)
      obs_count<-nrow(obs_data)
      cat(paste("get_obsdata run on x_obs for site",site,obs_count,"\n",sep=" "))
      countbyyr<-aggregate(obs_data$discharge, list(obs_data$wy_val), length)
      colnames(countbyyr)<-c('wy','num_samples')
      sub_countbyyr<-countbyyr[countbyyr$num_samples>=365,]
      if (nrow(sub_countbyyr)==0) {
        comment<-"No complete water years for site"
      } else {
        obs_data<-merge(obs_data,sub_countbyyr,by.x="wy_val",by.y="wy")
        obs_data<-obs_data[order(obs_data$date),]
        obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
        site_no <- rep(site,nrow(obs_data))
        obs_data <- cbind(site_no,obs_data,stringsAsFactors=FALSE)
      }} else {
        obs_data<-"No observed data for this site"
      }
    dataOut[[i]]<-obs_data
  }
  return(dataOut)
}