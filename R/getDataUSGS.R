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
#' getDataUSGS(sites,startdate,enddate)
getDataUSGS <- function(sites,startdate,enddate) {
  startdate <- paste(startdate,"10","01",sep="-")
  enddate <- paste(enddate,"09","30",sep="-")
  # Hardcode NWIS urls and parameters.
  nwisDvUrl = "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
  offering = "00003"
  property = "00060"
  drainage_url = "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
  interval<-''
  latest<-''
  dataOut <- list()
  for (i in 1:length(sites)) {
    site=sites[i]
    url2<-paste(nwisDvUrl,site,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,sep='')
    x_obs <- getXMLWML1.1Data(url2)
    
    if (nrow(x_obs)>2) {
      obs_data <- get_obsdata(x_obs)
      obs_count<-nrow(obs_data)
      cat(paste("get_obsdata run on x_obs for site",site,obs_count,"\n",sep=" "))
      drain_url<-paste(drainage_url,site,sep="")
      drain_area<-getDrainageArea(drain_url)
      cat(paste("data and drainage area retrieved for site",site,drain_area,"\n",sep=" "))
      countbyyr<-aggregate(obs_data$discharge, list(obs_data$wy_val), length)
      colnames(countbyyr)<-c('wy','num_samples')
      sub_countbyyr<-subset(countbyyr,num_samples >= 365)
      if (nrow(sub_countbyyr)==0) {
        comment<-"No complete water years for site"
      } else {
        obs_data<-merge(obs_data,sub_countbyyr,by.x="wy_val",by.y="wy")
        obs_data<-obs_data[order(obs_data$date),]
        obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
      }} else {
        obs_data<-"No observed data for this site"
      }
    dataOut[[i]]<-obs_data
  }
  return(dataOut)
}