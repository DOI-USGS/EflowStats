#' Function to transform data into the format required for calculation of HIT/HAT statistics
#' 
#' This function accepts a raw data frame pulled from the NWIS webservice and returns a data frame of 
#' observed data for use in calculating HIT/HAT statistics
#' 
#' @param x_obs data frame pulled from NWIS webservice
#' @return obs_data data frame of observed data for the chosen parameters
#' @export
#' @examples
#' url<-"http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
#' sites<-"02177000"
#' startdate<-"2011-10-01"
#' enddate<-"2012-09-30"
#' offering<-'00003'
#' property<-'00060'
#' obs_url<-paste0(url,sites,'&startDT=',startdate,'&endDT=',
#'             enddate,'&statCd=',offering,'&parameterCd=',property)
#' \dontrun{
#' x_obs <- getXMLWML1.1Data(obs_url)
#' obs_data <- get_obsdata(x_obs)
#' }
get_obsdata <- function(x_obs) {
  x2<-(x_obs$date)
  x_obs<-data.frame(x2,x_obs$discharge,stringsAsFactors=FALSE)
  colnames(x_obs)<-c("date","discharge")
  x_obs$month_val <- as.integer(substr(x_obs$date,6,7))
  x_obs$year_val <- as.integer(substr(x_obs$date,1,4))
  x_obs$day_val <- as.integer(substr(x_obs$date,9,10))
  x_obs$jul_val <- as.integer(strptime(x_obs$date,"%Y-%m-%d")$yday)+1
  x_obs$wy_val <- ifelse(as.numeric(x_obs$month_val)>=10,as.character(as.numeric(x_obs$year_val)+1),x_obs$year_val) 
  temp <- aggregate(discharge ~ wy_val,data=x_obs,length)
  temp <- temp[which(temp$discharge>=365),]
  
  obs_data<-x_obs[x_obs$wy_val %in% temp$wy_val,]
  return(obs_data)
}
