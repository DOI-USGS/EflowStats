#install.packages("NWCCompare",repos="http://usgs-r.github.com",type="source")
library(XML)
library(zoo)
library(chron)
library(doBy)
library(hydroGOF)
library(RCurl)
library(lmomco)
library(HITHATStats)
library(NWCCompare)

sos_url_temp="http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering_temp='00003'
property_temp='00060'
drainage_url="http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="

setwd('/Users/jlthomps/Documents/R/')
startdate<-"2008-10-01"
enddate<-"2013-9-30"
interval<-''
latest<-''
sites='02177000'
url2<-paste(sos_url_temp,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering_temp,'&parameterCd=',property_temp,sep='')
x_obs <- getXMLWML1.1Data(url2)
obs_data <- get_obsdata(x_obs)
obs_count<-nrow(obs_data)
cat(paste("get_obsdata run on x_obs for site",sites,obs_count,"\n",sep=" "))
drain_url<-paste(drainage_url,sites,sep="")
drain_area<-getDrainageArea(drain_url)
cat(paste("data and drainage area retrieved for site",sites,drain_area,"\n",sep=" "))
yv<-as.character(min(obs_data$date))
ymaxv<-as.character(max(obs_data$date))
cat(paste("dates calculated for site",sites,"\n",sep=" "))

obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
obs_count <- nrow(obs_data)
cat(paste("dfs created for site",sites,obs_count,"\n",sep=" "))

