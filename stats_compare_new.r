# wps.des: id=test_stats, title = test stats, abstract = Finds the mean daily flow median daily flow and skewness of daily flow in the input dataset;
# wps.in: model_url, string, SOS Endpoint, A fully formed SOS GetObservations request that will return a SWE common CSV block holding date and flow;

library(XML)
library(zoo)
library(chron)
library(doBy)
library(hydroGOF)
library(RCurl)
library(HITHATStats)
library(NWCCompare)

#model_url="http://cida.usgs.gov/gdp/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/afinch/afinch-Special-0.3.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
#model_url="http://cida.usgs.gov/gdp/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/stats/stats-Special-0.3.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
#model_url="http://cida.usgs.gov/gdp/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/waters/waters-Special-1.2.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
#model_url="http://cida.usgs.gov/nwc/thredds/sos/watersmart/stats/stats-SE-DENSE1-2.03.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
model_url="http://cida.usgs.gov/nwc/thredds/sos/watersmart/afinch/afinch-SE-SPARSE1-0.1.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
#model_url="http://cida.usgs.gov/gdp/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/waters/waters-Special-0.3.nc?request=GetObservation&service=SOS&version=1.0.0&offering"

sos_url_temp="http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering_temp='00003'
property_temp='00060'
drainage_url="http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="

scenario_url=paste(substr(model_url,1,regexpr("Get",model_url)-1),"GetCapabilities&service=SOS&version=1.0.0",sep="")

setwd('/Users/jlthomps/Documents/R/')
system("rm graph*png")
system("rm monthly*txt")
#a<-read.csv(header=F,colClasses=c("character"),text=sites)
#a2<-read.csv(header=F,colClasses=c("character"),text=modsites)
#a<-read.csv("sites_waters_stat.txt",header=F,colClasses=c("character"))
#a2<-read.csv("sites_waters_stat.txt",header=F,colClasses=c("character"))
getcap<-getScenarioSites(scenario_url)
modprop<-getcap$modprop
a<-t(getcap$scenario_sites)
a2<-a
al<-length(a)

comment<-vector(length=al)
ObsFlowStats <- matrix(nrow=al,ncol=138)
ModFlowStats <- matrix(nrow=nrow(ObsFlowStats),ncol=ncol(ObsFlowStats))
GoFMetrics <- matrix(nrow=nrow(ObsFlowStats),ncol=108)
MonAnnGoF <- matrix(nrow=nrow(ObsFlowStats),ncol=84)
yv<-vector(length=al)
ymaxv<-vector(length=al)

for (i in 1:length(a2)){
  modsites<-a2[i]
  url<-paste(model_url,'=',modsites,'&observedProperty=',modprop,sep='',collapse=NULL)
  x_mod<-SWE_CSV_IHA(url)
  if (length(sapply(x_mod,nchar))>1) {
    startdate<-min(x_mod$date)
    enddate<-max(x_mod$date)
    interval<-''
    latest<-''
    sites=a[i]
    url2<-paste(sos_url_temp,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering_temp,'&parameterCd=',property_temp,'&access=3',sep='')
    x_obs <- getXMLWML1.1Data(url2)

    if (nrow(x_obs)>2) {
      obs_data <- get_obsdata(x_obs)
      x_mod$date <- as.Date(x_mod$date,format="%Y-%m-%d")
      x_mod<-x_mod[x_mod$date>=min(x_obs$date) & x_mod$date<=max(x_obs$date), ]
      drain_url<-paste(drainage_url,sites,sep="")
      drain_area<-getDrainageArea(drain_url)
      mod_data <- get_obsdata(x_mod)

      countbyyr<-aggregate(obs_data$discharge, list(obs_data$wy_val), length)
      countbyyr_mod<-aggregate(mod_data$discharge, list(mod_data$wy_val), length)
      colnames(countbyyr)<-c('wy','num_samples')
      colnames(countbyyr_mod)<-c('wy','num_samples')
      sub_countbyyr<-subset(countbyyr,num_samples >= 365)
      sub_countbyyr_mod<-subset(countbyyr_mod,num_samples >= 365)
      include_yrs<-merge(sub_countbyyr,sub_countbyyr_mod)
      obs_data<-merge(obs_data,include_yrs,by.x="wy_val",by.y="wy")
      mod_data<-merge(mod_data,include_yrs,by.x="wy_val",by.y="wy")
      if (length(mod_data$discharge)<3) { 
          comment[i]<-"No matching complete water years for site" 
      } else { 
      if (length(mod_data$discharge)!=length(obs_data$discharge)) { 
        comment[i]<-"Observed and modeled time-series don't match for site"
      } else {

      yv[i]<-as.character(min(obs_data$date))
      ymaxv[i]<-as.character(max(obs_data$date))
      x_modz<-mod_data$discharge
      x_obsz<-obs_data$discharge
      dates<-as.Date(obs_data$date)
      file<-paste("graph",toString(sites),".png",sep="")
      #png(file)
      ggof(x_modz,x_obsz,na.rm=FALSE,dates,main=modsites)
      dev.copy(png,file)
      dev.off()
      file<-paste("monthly_mean_ts_obs",toString(sites),".txt",sep="")
      monthly_mean<-monthly.mean.ts(obs_data)
      write.table(monthly_mean,file=file,col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
      file<-paste("monthly_mean_ts_mod",toString(sites),".txt",sep="")
      monthly_mean<-monthly.mean.ts(mod_data)
      write.table(monthly_mean,file=file,col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
      
      obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
      mod_data <- mod_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
      ObsFlowStats[i,] <- FlowStats(obs_data,drain_area)
      ModFlowStats[i,] <- FlowStats(mod_data,drain_area)
      comment <- ""
      GoFMetrics[i,] <- SiteGoF(obs_data,mod_data)
      MonAnnGoF[i,] <- MonthlyAnnualGoF(obs_data,mod_data)
    }
  }
  } else {
    comment[i]<-"No observed data for this site"
  }
  } else { 
    comment[i]<-"No modeled data for site"
  } 
}

FlowStats.PDiff <- (ModFlowStats-ObsFlowStats)/ObsFlowStats
FlowStats.GoF <- RegionalGoF(ObsFlowStats,ModFlowStats)

statsout<-data.frame(a,yv,ymaxv,GoFMetrics,MonAnnGoF,ObsFlowStats,ModFlowStats,FlowStats.PDiff,comment)
#colnames(statsout)<-c('site_no','min_date','max_date',,'comment')  !WILL UPDATE NAMES IN FUTURE!

output="output.zip"
if (i==length(a2)) {
  write.table(statsout,file="output.txt",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
  system("rm output.zip")
  system("zip -r output graph*png")
  system("zip -r output monthly*txt")
  system("zip -r output output*")
} else { 
  output="output.zip" 
  message<-"One or more web service calls resulted in failure. Please try again."
  write.table(message,file="output.txt",col.names=FALSE,row.names=FALSE,quote=FALSE)
}

# wps.out: output, zip, output_file, A file containing the mean daily flow median daily flow and skewness of daily flow;
