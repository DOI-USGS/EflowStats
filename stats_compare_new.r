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
a<-t(getcap$scenario_sites[1:10])
a2<-a
al<-length(a)

comment<-vector(length=al)
ObsFlowStats <- matrix(nrow=al,ncol=28)
ModFlowStats <- matrix(nrow=nrow(ObsFlowStats),ncol=ncol(ObsFlowStats))
magnifSevenObs <- matrix(nrow=nrow(ObsFlowStats),ncol=7)
magnifSevenMod <- matrix(nrow=nrow(ObsFlowStats),ncol=7)
GoFMetrics <- matrix(nrow=nrow(ObsFlowStats),ncol=108)
#MonAnnGoF <- matrix(nrow=nrow(ObsFlowStats),ncol=84)
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
      magnifSevenObs[i,] <- magnifSeven(obs_data)
      magnifSevenMod[i,] <- magnifSeven(mod_data)
      comment <- ""
      GoFMetrics[i,] <- SiteGoF(obs_data,mod_data)
#      MonAnnGoF[i,] <- MonthlyAnnualGoF(obs_data,mod_data)
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
magnifSeven.PDiff <- (magnifSevenMod-magnifSevenObs)/magnifSevenObs
#FlowStats.GoF <- RegionalGoF(ObsFlowStats,ModFlowStats)

statsout<-data.frame(t(a),yv,ymaxv,GoFMetrics,magnifSevenObs,ObsFlowStats,magnifSevenMod,ModFlowStats,magnifSeven.PDiff,FlowStats.PDiff,comment,stringsAsFactors=FALSE)
colnames(statsout)<-c('site_no','min_date','max_date','nsev','nselogv','rmsev','pbiasv','pearsonv','spearmanv',
              'nsev_90','nsev_75_90','nsev_50_75','nsev_25_50','nsev_10_25','nsev_10',
              'rmsev_90','rmsev_75_90','rmsev_50_75','rmsev_25_50','rmsev_10_25','rmsev_10',
              'pbiasv_90','pbiasv_75_90','pbiasv_50_75','pbiasv_25_50','pbiasv_10_25','pbiasv_10',           
              'pearsonv_90','pearsonv_75_90','pearsonv_50_75','pearsonv_25_50','pearsonv_10_25','pearsonv_10',
              'spearmanv_90','spearmanv_75_90','spearmanv_50_75','spearmanv_25_50','spearmanv_10_25','spearmanv_10',
              'NSEbyMonthJan','NSELOGbyMonthJan','RMSEbyMonthJan','BiasbyMonthJan','PearsonbyMonthJan','SpearmanbyMonthJan',
              'NSEbyMonthFeb','NSELOGbyMonthFeb','RMSEbyMonthFeb','BiasbyMonthFeb','PearsonbyMonthFeb','SpearmanbyMonthFeb',
              'NSEbyMonthMar','NSELOGbyMonthMar','RMSEbyMonthMar','BiasbyMonthMar','PearsonbyMonthMar','SpearmanbyMonthMar',
              'NSEbyMonthApr','NSELOGbyMonthApr','RMSEbyMonthApr','BiasbyMonthApr','PearsonbyMonthApr','SpearmanbyMonthApr',
              'NSEbyMonthMay','NSELOGbyMonthMay','RMSEbyMonthMay','BiasbyMonthMay','PearsonbyMonthMay','SpearmanbyMonthMay',
              'NSEbyMonthJun','NSELOGbyMonthJun','RMSEbyMonthJun','BiasbyMonthJun','PearsonbyMonthJun','SpearmanbyMonthJun',
              'NSEbyMonthJul','NSELOGbyMonthJul','RMSEbyMonthJul','BiasbyMonthJul','PearsonbyMonthJul','SpearmanbyMonthJul',
              'NSEbyMonthAug','NSELOGbyMonthAug','RMSEbyMonthAug','BiasbyMonthAug','PearsonbyMonthAug','SpearmanbyMonthAug',
              'NSEbyMonthSep','NSELOGbyMonthSep','RMSEbyMonthSep','BiasbyMonthSep','PearsonbyMonthSep','SpearmanbyMonthSep',
              'NSEbyMonthOct','NSELOGbyMonthOct','RMSEbyMonthOct','BiasbyMonthOct','PearsonbyMonthOct','SpearmanbyMonthOct',
              'NSEbyMonthNov','NSELOGbyMonthNov','RMSEbyMonthNov','BiasbyMonthNov','PearsonbyMonthNov','SpearmanbyMonthNov',
              'NSEbyMonthDec','NSELOGbyMonthDec','RMSEbyMonthDec','BiasbyMonthDec','PearsonbyMonthDec','SpearmanbyMonthDec',
              'lam1Obs','tau2Obs','tau3Obs','tau4Obs','ar1Obs','amplitudeObs','phaseObs',
              'med_flowObs','cv_flowObs','cv_dailyObs','ma26Obs','ma41Obs','ml18Obs','ml20Obs',
              'mh10Obs','fl2Obs','fh6Obs','fh7Obs','dl6Obs','dh13Obs','dh16Obs','ta1Obs','tl1Obs','th1Obs','ra5Obs','ra7Obs','ra8Obs',
              'l7Q10Obs','l7Q2Obs','return_10Obs','flow_10Obs','flow_25Obs','flow_50Obs','flow_75Obs','flow_90Obs',
              'lam1Mod','tau2Mod','tau3Mod','tau4Mod','ar1Mod','amplitudeMod','phaseMod',
              'med_flowMod','cv_flowMod','cv_dailyMod','ma26Mod','ma41Mod','ml18Mod','ml20Mod',
              'mh10Mod','fl2Mod','fh6Mod','fh7Mod','dl6Mod','dh13Mod','dh16Mod','ta1Mod','tl1Mod','th1Mod','ra5Mod','ra7Mod','ra8Mod',
              'l7Q10Mod','l7Q2Mod','return_10Mod','flow_10Mod','flow_25Mod','flow_50Mod','flow_75Mod','flow_90Mod',
              'lam1Diff','tau2Diff','tau3Diff','tau4Diff','ar1Diff','amplitudeDiff','phaseDiff',
              'med_flowDiff','cv_flowDiff','cv_dailyDiff','ma26Diff','ma41Diff','ml18Diff','ml20Diff',
              'mh10Diff','fl2Diff','fh6Diff','fh7Diff','dl6Diff','dh13Diff','dh16Diff','ta1Diff','tl1Diff','th1Diff','ra5Diff','ra7Diff','ra8Diff',
              'l7Q10Diff','l7Q2Diff','return_10Diff','flow_10Diff','flow_25Diff','flow_50Diff','flow_75Diff','flow_90Diff','comment')  

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

# wps.out: output, zip, output_file, A file containing the table of statistics as well as monthly stats and graphs for each site;
