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
#' startdate<-"2012-09-01"
#' enddate<-"2012-10-01"
#' offering<-'00003'
#' property<-'00060'
#' obs_url<-paste(url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,sep='')
#' x_obs <- getXMLWML1.1Data(obs_url)
#' get_obsdata(x_obs)
get_obsdata <- function(x_obs) {
x2<-(x_obs$date)
x_obs<-data.frame(strptime(x2, "%Y-%m-%d"),x_obs$discharge)
colnames(x_obs)<-c("date","discharge")
selqfile<-x_obs
tempdatafr<-NULL
tempdatafr<-data.frame(selqfile)
month_val<-rep(0,length(tempdatafr$date))
year_val<-rep(0,length(tempdatafr$date))
day_val<-rep(0,length(tempdatafr$date))
jul_val<-rep(0,length(tempdatafr$date))
wy_val<-rep(0,length(tempdatafr$date))
ones_val<-rep(1,length(tempdatafr$date))
qfiletempf<-data.frame(tempdatafr$date,tempdatafr$discharge,month_val,year_val,day_val,jul_val,wy_val)
colnames(qfiletempf)<-c('date','discharge','month_val','year_val','day_val','jul_val','wy_val')
qfiletempf$month_val<-substr(x_obs$date,6,7)
as.numeric(qfiletempf$month_val)
qfiletempf$year_val<-substr(x_obs$date,1,4)
as.numeric(qfiletempf$year_val)
qfiletempf$day_val<-substr(x_obs$date,9,10)
as.numeric(qfiletempf$day_val)
qfiletempf$jul_val<-strptime(x_obs$date, "%Y-%m-%d")$yday+1
as.numeric(qfiletempf$jul_val)
qfiletempf$wy_val<-ifelse(as.numeric(qfiletempf$month_val)>=10,as.character(as.numeric(qfiletempf$year_val)+ones_val),qfiletempf$year_val) 

countbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), length)
colnames(countbyyr)<-c('wy','num_samples')
sub_countbyyr<-subset(countbyyr,countbyyr$num_samples >= 365)
obs_data<-merge(qfiletempf,sub_countbyyr,by.x="wy_val",by.y="wy")
return(obs_data)
}
