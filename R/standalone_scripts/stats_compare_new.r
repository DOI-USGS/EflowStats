# wps.des: id=test_stats, title = test stats, abstract = Finds the mean daily flow median daily flow and skewness of daily flow in the input dataset;
# wps.in: model_url, string, SOS Endpoint, A fully formed SOS GetObservations request that will return a SWE common CSV block holding date and flow;

library(XML)
library(zoo)
library(chron)
library(doBy)
library(hydroGOF)
library(HITHATStats)
library(NWCCompare)

#model_url="http://cida.usgs.gov/gdp/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/afinch/afinch-Special-0.3.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
#model_url="http://cida.usgs.gov/gdp/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/stats/stats-Special-0.3.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
#model_url="http://cida.usgs.gov/gdp/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/waters/waters-Special-1.2.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
#model_url="http://cida.usgs.gov/nwc/thredds/sos/watersmart/stats/stats-SE-DENSE1-2.03.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
model_url="http://cida.usgs.gov/nwc/thredds/sos/watersmart/afinch/afinch-SE-SPARSE1-0.1.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
#model_url="http://cida.usgs.gov/gdp/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/waters/waters-Special-0.3.nc?request=GetObservation&service=SOS&version=1.0.0&offering"

sos_url_temp="http://waterservices.usgs.gov/nwis/dv/?format=rdb,1.0&sites="
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
nsev<-vector(length=al)
nselogv<-vector(length=al)
rmsev<-vector(length=al)
nsev_90<-vector(length=al)
nsev_75_90<-vector(length=al)
nsev_50_75<-vector(length=al)
nsev_25_50<-vector(length=al)
nsev_10_25<-vector(length=al)
nsev_10<-vector(length=al)
rmsev_90<-vector(length=al)
rmsev_75_90<-vector(length=al)
rmsev_50_75<-vector(length=al)
rmsev_25_50<-vector(length=al)
rmsev_10_25<-vector(length=al)
rmsev_10<-vector(length=al)
yv<-vector(length=al)
ma12v<-vector(length=al)
ma13v<-vector(length=al)
ma14v<-vector(length=al)
ma15v<-vector(length=al)
ma16v<-vector(length=al)
ma17v<-vector(length=al)
ma18v<-vector(length=al)
ma19v<-vector(length=al)
ma20v<-vector(length=al)
ma21v<-vector(length=al)
ma22v<-vector(length=al)
ma23v<-vector(length=al)
ml1v<-vector(length=al)
ml2v<-vector(length=al)
ml3v<-vector(length=al)
ml4v<-vector(length=al)
ml5v<-vector(length=al)
ml6v<-vector(length=al)
ml7v<-vector(length=al)
ml8v<-vector(length=al)
ml9v<-vector(length=al)
ml10v<-vector(length=al)
ml11v<-vector(length=al)
ml12v<-vector(length=al)
mh1v<-vector(length=al)
mh2v<-vector(length=al)
mh3v<-vector(length=al)
mh4v<-vector(length=al)
mh5v<-vector(length=al)
mh6v<-vector(length=al)
mh7v<-vector(length=al)
mh8v<-vector(length=al)
mh9v<-vector(length=al)
mh10v<-vector(length=al)
mh11v<-vector(length=al)
mh12v<-vector(length=al)
l7Q10v<-vector(length=al)
l7Q2v<-vector(length=al)
return_10v<-vector(length=al)
modsites<-a2
sites<-a
yv<-vector(length=al)
ymaxv<-vector(length=al)
ma12v2<-vector(length=al)
ma13v2<-vector(length=al)
ma14v2<-vector(length=al)
ma15v2<-vector(length=al)
ma16v2<-vector(length=al)
ma17v2<-vector(length=al)
ma18v2<-vector(length=al)
ma19v2<-vector(length=al)
ma20v2<-vector(length=al)
ma21v2<-vector(length=al)
ma22v2<-vector(length=al)
ma23v2<-vector(length=al)
ml1v2<-vector(length=al)
ml2v2<-vector(length=al)
ml3v2<-vector(length=al)
ml4v2<-vector(length=al)
ml5v2<-vector(length=al)
ml6v2<-vector(length=al)
ml7v2<-vector(length=al)
ml8v2<-vector(length=al)
ml9v2<-vector(length=al)
ml10v2<-vector(length=al)
ml11v2<-vector(length=al)
ml12v2<-vector(length=al)
mh1v2<-vector(length=al)
mh2v2<-vector(length=al)
mh3v2<-vector(length=al)
mh4v2<-vector(length=al)
mh5v2<-vector(length=al)
mh6v2<-vector(length=al)
mh7v2<-vector(length=al)
mh8v2<-vector(length=al)
mh9v2<-vector(length=al)
mh10v2<-vector(length=al)
mh11v2<-vector(length=al)
mh12v2<-vector(length=al)
l7Q10v2<-vector(length=al)
l7Q2v2<-vector(length=al)
return_10v2<-vector(length=al)

ma12diff<-vector(length=al)
ma13diff<-vector(length=al)
ma14diff<-vector(length=al)
ma15diff<-vector(length=al)
ma16diff<-vector(length=al)
ma17diff<-vector(length=al)
ma18diff<-vector(length=al)
ma19diff<-vector(length=al)
ma20diff<-vector(length=al)
ma21diff<-vector(length=al)
ma22diff<-vector(length=al)
ma23diff<-vector(length=al)
ml1diff<-vector(length=al)
ml2diff<-vector(length=al)
ml3diff<-vector(length=al)
ml4diff<-vector(length=al)
ml5diff<-vector(length=al)
ml6diff<-vector(length=al)
ml7diff<-vector(length=al)
ml8diff<-vector(length=al)
ml9diff<-vector(length=al)
ml10diff<-vector(length=al)
ml11diff<-vector(length=al)
ml12diff<-vector(length=al)
mh1diff<-vector(length=al)
mh2diff<-vector(length=al)
mh3diff<-vector(length=al)
mh4diff<-vector(length=al)
mh5diff<-vector(length=al)
mh6diff<-vector(length=al)
mh7diff<-vector(length=al)
mh8diff<-vector(length=al)
mh9diff<-vector(length=al)
mh10diff<-vector(length=al)
mh11diff<-vector(length=al)
mh12diff<-vector(length=al)
l7Q10diff<-vector(length=al)
l7Q2diff<-vector(length=al)
return_10diff<-vector(length=al)
comment<-vector(length=al)
mean_flow<-vector(length=al)
med_flow<-vector(length=al)
cv_flow<-vector(length=al)
cv_daily<-vector(length=al)
skew_daily<-vector(length=al)
mean_flow_mod<-vector(length=al)
med_flow_mod<-vector(length=al)
cv_flow_mod<-vector(length=al)
cv_daily_mod<-vector(length=al)
skew_daily_mod<-vector(length=al)
mean_flow_diff<-vector(length=al)
med_flow_diff<-vector(length=al)
cv_flow_diff<-vector(length=al)
cv_daily_diff<-vector(length=al)
skew_daily_diff<-vector(length=al)
flow_10_obs<-vector(length=al)
flow_25_obs<-vector(length=al)
flow_50_obs<-vector(length=al)
flow_75_obs<-vector(length=al)
flow_90_obs<-vector(length=al)
flow_10_mod<-vector(length=al)
flow_25_mod<-vector(length=al)
flow_50_mod<-vector(length=al)
flow_75_mod<-vector(length=al)
flow_90_mod<-vector(length=al)
flow_10_diff<-vector(length=al)
flow_25_diff<-vector(length=al)
flow_50_diff<-vector(length=al)
flow_75_diff<-vector(length=al)
flow_90_diff<-vector(length=al)
pbiasv<-vector(length=al)
pbiasv_90<-vector(length=al)
pbiasv_75_90<-vector(length=al)
pbiasv_50_75<-vector(length=al)
pbiasv_25_50<-vector(length=al)
pbiasv_10_25<-vector(length=al)
pbiasv_10<-vector(length=al)


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
    x_obs <- retrieveNWISData(url2)

    if (nrow(x_obs)>2) {
      obs_data <- get_obsdata(x_obs)
      x_mod<-x_mod[x_mod$date>=min(obs_data$date) & x_mod$date<=max(obs_data$date), ]
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
      pbiasv[i]<-pbias(x_modz,x_obsz)
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

      dfcvbyyrf <- meanflowbyyear(obs_data)
      dfcvbyyrf_mod <- meanflowbyyear(mod_data)
      mean_flow[i]<-mean(dfcvbyyrf$meanq,na.rm=TRUE)
      med_flow[i]<-median(dfcvbyyrf$meanq,na.rm=TRUE)
      cv_flow[i]<-sd(dfcvbyyrf$meanq,na.rm=TRUE)/mean(dfcvbyyrf$meanq,na.rm=TRUE)
      cv_daily[i]<-cv(obs_data)
      skew_daily[i]<-skew(obs_data)
      mean_flow_mod[i]<-mean(dfcvbyyrf_mod$meanq,na.rm=TRUE)
      med_flow_mod[i]<-median(dfcvbyyrf_mod$meanq,na.rm=TRUE)
      cv_flow_mod[i]<-sd(dfcvbyyrf_mod$meanq,na.rm=TRUE)/mean(dfcvbyyrf_mod$meanq,na.rm=TRUE)
      cv_daily_mod[i]<-cv(mod_data)
      skew_daily_mod[i]<-skew(mod_data)
      ma12v[i]<-ma12.23(obs_data)[1:1,2:2]
      ma13v[i]<-ma12.23(obs_data)[2:2,2:2]
      ma14v[i]<-ma12.23(obs_data)[3:3,2:2]
      ma15v[i]<-ma12.23(obs_data)[4:4,2:2]
      ma16v[i]<-ma12.23(obs_data)[5:5,2:2]
      ma17v[i]<-ma12.23(obs_data)[6:6,2:2]
      ma18v[i]<-ma12.23(obs_data)[7:7,2:2]
      ma19v[i]<-ma12.23(obs_data)[8:8,2:2]
      ma20v[i]<-ma12.23(obs_data)[9:9,2:2]
      ma21v[i]<-ma12.23(obs_data)[10:10,2:2]
      ma22v[i]<-ma12.23(obs_data)[11:11,2:2]
      ma23v[i]<-ma12.23(obs_data)[12:12,2:2]
      ml1v[i]<-unlist(ml1.12(obs_data)[1])
      ml2v[i]<-unlist(ml1.12(obs_data)[2])
      ml3v[i]<-unlist(ml1.12(obs_data)[3])
      ml4v[i]<-unlist(ml1.12(obs_data)[4])
      ml5v[i]<-unlist(ml1.12(obs_data)[5])
      ml6v[i]<-unlist(ml1.12(obs_data)[6])
      ml7v[i]<-unlist(ml1.12(obs_data)[7])
      ml8v[i]<-unlist(ml1.12(obs_data)[8])
      ml9v[i]<-unlist(ml1.12(obs_data)[9])
      ml10v[i]<-unlist(ml1.12(obs_data)[10])
      ml11v[i]<-unlist(ml1.12(obs_data)[11])
      ml12v[i]<-unlist(ml1.12(obs_data)[12])
      mh1v[i]<-unlist(mh1.12(obs_data)[1])
      mh2v[i]<-unlist(mh1.12(obs_data)[2])
      mh3v[i]<-unlist(mh1.12(obs_data)[3])
      mh4v[i]<-unlist(mh1.12(obs_data)[4])
      mh5v[i]<-unlist(mh1.12(obs_data)[5])
      mh6v[i]<-unlist(mh1.12(obs_data)[6])
      mh7v[i]<-unlist(mh1.12(obs_data)[7])
      mh8v[i]<-unlist(mh1.12(obs_data)[8])
      mh9v[i]<-unlist(mh1.12(obs_data)[9])
      mh10v[i]<-unlist(mh1.12(obs_data)[10])
      mh11v[i]<-unlist(mh1.12(obs_data)[11])
      mh12v[i]<-unlist(mh1.12(obs_data)[12])
      l7Q10v[i]<-l7Q10(obs_data)
      l7Q2v[i]<-l7Q2(obs_data)
      return_10v[i]<-return_10(obs_data)

      ma12v2[i]<-ma12.23(mod_data)[1:1,2:2]
      ma13v2[i]<-ma12.23(mod_data)[2:2,2:2]
      ma14v2[i]<-ma12.23(mod_data)[3:3,2:2]
      ma15v2[i]<-ma12.23(mod_data)[4:4,2:2]
      ma16v2[i]<-ma12.23(mod_data)[5:5,2:2]
      ma17v2[i]<-ma12.23(mod_data)[6:6,2:2]
      ma18v2[i]<-ma12.23(mod_data)[7:7,2:2]
      ma19v2[i]<-ma12.23(mod_data)[8:8,2:2]
      ma20v2[i]<-ma12.23(mod_data)[9:9,2:2]
      ma21v2[i]<-ma12.23(mod_data)[10:10,2:2]
      ma22v2[i]<-ma12.23(mod_data)[11:11,2:2]
      ma23v2[i]<-ma12.23(mod_data)[12:12,2:2]
      ml1v2[i]<-unlist(ml1.12(mod_data)[1])
      ml2v2[i]<-unlist(ml1.12(mod_data)[2])
      ml3v2[i]<-unlist(ml1.12(mod_data)[3])
      ml4v2[i]<-unlist(ml1.12(mod_data)[4])
      ml5v2[i]<-unlist(ml1.12(mod_data)[5])
      ml6v2[i]<-unlist(ml1.12(mod_data)[6])
      ml7v2[i]<-unlist(ml1.12(mod_data)[7])
      ml8v2[i]<-unlist(ml1.12(mod_data)[8])
      ml9v2[i]<-unlist(ml1.12(mod_data)[9])
      ml10v2[i]<-unlist(ml1.12(mod_data)[10])
      ml11v2[i]<-unlist(ml1.12(mod_data)[11])
      ml12v2[i]<-unlist(ml1.12(mod_data)[12])
      mh1v2[i]<-unlist(mh1.12(mod_data)[1])
      mh2v2[i]<-unlist(mh1.12(mod_data)[2])
      mh3v2[i]<-unlist(mh1.12(mod_data)[3])
      mh4v2[i]<-unlist(mh1.12(mod_data)[4])
      mh5v2[i]<-unlist(mh1.12(mod_data)[5])
      mh6v2[i]<-unlist(mh1.12(mod_data)[6])
      mh7v2[i]<-unlist(mh1.12(mod_data)[7])
      mh8v2[i]<-unlist(mh1.12(mod_data)[8])
      mh9v2[i]<-unlist(mh1.12(mod_data)[9])
      mh10v2[i]<-unlist(mh1.12(mod_data)[10])
      mh11v2[i]<-unlist(mh1.12(mod_data)[11])
      mh12v2[i]<-unlist(mh1.12(mod_data)[12])
      l7Q10v2[i]<-l7Q10(mod_data)
      l7Q2v2[i]<-l7Q2(mod_data)
      return_10v2[i]<-return_10(mod_data)
      comment[i]<-""

      nsev[i]<-nse(obs_data$discharge,mod_data$discharge)
      nselogv[i]<-nselog(obs_data$discharge,mod_data$discharge)
      rmsev[i]<-rmse(obs_data$discharge,mod_data$discharge)
      sort_x_obs<-sort(obs_data$discharge)
      sort_x_mod<-sort(mod_data$discharge)
      rank_10<-floor(findrank(length(sort_x_mod),0.10))
      rank_25<-floor(findrank(length(sort_x_mod),0.25))
      rank_50<-floor(findrank(length(sort_x_mod),0.5))
      rank_75<-floor(findrank(length(sort_x_mod),0.75))
      rank_90<-floor(findrank(length(sort_x_mod),0.9))
      nsev_90[i]<-nse(sort_x_obs[1:rank_90],sort_x_mod[1:rank_90])
      nsev_75_90[i]<-nse(sort_x_obs[rank_90:rank_75],sort_x_mod[rank_90:rank_75])
      nsev_50_75[i]<-nse(sort_x_obs[rank_75:rank_50],sort_x_mod[rank_75:rank_50])
      nsev_25_50[i]<-nse(sort_x_obs[rank_50:rank_25],sort_x_mod[rank_50:rank_25])
      nsev_10_25[i]<-nse(sort_x_obs[rank_25:rank_10],sort_x_mod[rank_25:rank_10])
      nsev_10[i]<-nse(sort_x_obs[rank_10:length(sort_x_mod)],sort_x_mod[rank_10:length(sort_x_mod)])
      rmsev_90[i]<-rmse(sort_x_obs[1:rank_90],sort_x_mod[1:rank_90])
      rmsev_75_90[i]<-rmse(sort_x_obs[rank_90:rank_75],sort_x_mod[rank_90:rank_75])
      rmsev_50_75[i]<-rmse(sort_x_obs[rank_75:rank_50],sort_x_mod[rank_75:rank_50])
      rmsev_25_50[i]<-rmse(sort_x_obs[rank_50:rank_25],sort_x_mod[rank_50:rank_25])
      rmsev_10_25[i]<-rmse(sort_x_obs[rank_25:rank_10],sort_x_mod[rank_25:rank_10])
      rmsev_10[i]<-rmse(sort_x_obs[rank_10:length(sort_x_mod)],sort_x_mod[rank_10:length(sort_x_mod)])
      pbiasv_90[i]<-pbias(sort_x_obs[1:rank_90],sort_x_mod[1:rank_90])
      pbiasv_75_90[i]<-pbias(sort_x_obs[rank_90:rank_75],sort_x_mod[rank_90:rank_75])
      pbiasv_50_75[i]<-pbias(sort_x_obs[rank_75:rank_50],sort_x_mod[rank_75:rank_50])
      pbiasv_25_50[i]<-pbias(sort_x_obs[rank_50:rank_25],sort_x_mod[rank_50:rank_25])
      pbiasv_10_25[i]<-pbias(sort_x_obs[rank_25:rank_10],sort_x_mod[rank_25:rank_10])
      pbiasv_10[i]<-pbias(sort_x_obs[rank_10:length(sort_x_mod)],sort_x_mod[rank_10:length(sort_x_mod)])
      flow_10_obs[i]<-sort_x_obs[rank_10]
      flow_25_obs[i]<-sort_x_obs[rank_25]
      flow_50_obs[i]<-sort_x_obs[rank_50]
      flow_75_obs[i]<-sort_x_obs[rank_75]
      flow_90_obs[i]<-sort_x_obs[rank_90]
      flow_10_mod[i]<-sort_x_mod[rank_10]
      flow_25_mod[i]<-sort_x_mod[rank_25]
      flow_50_mod[i]<-sort_x_mod[rank_50]
      flow_75_mod[i]<-sort_x_mod[rank_75]
      flow_90_mod[i]<-sort_x_mod[rank_90]
    }
  }
  } else {
    comment[i]<-"No observed data for this site"
  }
  } else { 
    comment[i]<-"No modeled data for site"
  } 
}

ma12vdiff<-abs(ma12v-ma12v2)
ma13vdiff<-abs(ma13v-ma13v2)
ma14vdiff<-abs(ma14v-ma14v2)
ma15vdiff<-abs(ma15v-ma15v2)
ma16vdiff<-abs(ma16v-ma16v2)
ma17vdiff<-abs(ma17v-ma17v2)
ma18vdiff<-abs(ma18v-ma18v2)
ma19vdiff<-abs(ma19v-ma19v2)
ma20vdiff<-abs(ma20v-ma20v2)
ma21vdiff<-abs(ma21v-ma21v2)
ma22vdiff<-abs(ma22v-ma22v2)
ma23vdiff<-abs(ma23v-ma23v2)
ml1vdiff<-abs(ml1v-ml1v2)
ml2vdiff<-abs(ml2v-ml2v2)
ml3vdiff<-abs(ml3v-ml3v2)
ml4vdiff<-abs(ml4v-ml4v2)
ml5vdiff<-abs(ml5v-ml5v2)
ml6vdiff<-abs(ml6v-ml6v2)
ml7vdiff<-abs(ml7v-ml7v2)
ml8vdiff<-abs(ml8v-ml8v2)
ml9vdiff<-abs(ml9v-ml9v2)
ml10vdiff<-abs(ml10v-ml10v2)
ml11vdiff<-abs(ml11v-ml11v2)
ml12vdiff<-abs(ml12v-ml12v2)
mh1vdiff<-abs(mh1v-mh1v2)
mh2vdiff<-abs(mh2v-mh2v2)
mh3vdiff<-abs(mh3v-mh3v2)
mh4vdiff<-abs(mh4v-mh4v2)
mh5vdiff<-abs(mh5v-mh5v2)
mh6vdiff<-abs(mh6v-mh6v2)
mh7vdiff<-abs(mh7v-mh7v2)
mh8vdiff<-abs(mh8v-mh8v2)
mh9vdiff<-abs(mh9v-mh9v2)
mh10vdiff<-abs(mh10v-mh10v2)
mh11vdiff<-abs(mh11v-mh11v2)
mh12vdiff<-abs(mh12v-mh12v2)
l7Q10diff<-abs(l7Q10v-l7Q10v2)
l7Q2diff<-abs(l7Q2v-l7Q2v2)
return_10diff<-abs(return_10v-return_10v2)
mean_flow_diff<-abs(mean_flow-mean_flow_mod)
med_flow_diff<-abs(med_flow-med_flow_mod)
cv_flow_diff<-abs(cv_flow-cv_flow_mod)
cv_daily_diff<-abs(cv_daily-cv_daily_mod)
skew_daily_diff<-abs(skew_daily-skew_daily_mod)
flow_10_diff<-abs(flow_10_obs-flow_10_mod)
flow_25_diff<-abs(flow_25_obs-flow_25_mod)
flow_50_diff<-abs(flow_50_obs-flow_50_mod)
flow_75_diff<-abs(flow_75_obs-flow_75_mod)
flow_90_diff<-abs(flow_90_obs-flow_90_mod)

statsout<-data.frame(t(a),nsev,nselogv,rmsev,yv,ymaxv,mean_flow,med_flow,cv_flow,
                     nsev_90,nsev_75_90,nsev_50_75,nsev_25_50,nsev_10_25,nsev_10,
                     rmsev_90,rmsev_75_90,rmsev_50_75,rmsev_25_50,rmsev_10_25,rmsev_10,
                     pbiasv_90,pbiasv_75_90,pbiasv_50_75,pbiasv_25_50,pbiasv_10_25,pbiasv_10,
                     flow_10_obs,flow_25_obs,flow_50_obs,flow_75_obs,flow_90_obs,
                     cv_daily,skew_daily,ma12v,ma13v,
                     ma14v,ma15v,ma16v,ma17v,ma18v,ma19v,ma20v,
                     ma21v,ma22v,ma23v,ml1v,ml2v,ml3v,ml4v,ml5v,ml6v,ml7v,ml8v,ml9v,
                     ml10v,ml11v,ml12v,mh1v,mh2v,mh3v,mh4v,mh5v,
                     mh6v,mh7v,mh8v,mh9v,mh10v,mh11v,mh12v,l7Q10v,l7Q2v,return_10v,
                     mean_flow_mod,med_flow_mod,cv_flow_mod,cv_daily_mod,skew_daily_mod,
                     flow_10_mod,flow_25_mod,flow_50_mod,flow_75_mod,flow_90_mod,
                     ma12v2,ma13v2,
                     ma14v2,ma15v2,ma16v2,ma17v2,ma18v2,ma19v2,ma20v2,
                     ma21v2,ma22v2,ma23v2,ml1v2,ml2v2,ml3v2,ml4v2,ml5v2,ml6v2,ml7v2,ml8v2,ml9v2,
                     ml10v2,ml11v2,ml12v2,mh1v2,mh2v2,mh3v2,mh4v2,mh5v2,
                     mh6v2,mh7v2,mh8v2,mh9v2,mh10v2,mh11v2,mh12v2,l7Q10v2,l7Q2v2,return_10v2,mean_flow_diff,med_flow_diff,cv_flow_diff,cv_daily_diff,
                     skew_daily_diff,flow_10_diff,flow_25_diff,flow_50_diff,flow_75_diff,flow_90_diff,
                     ma12vdiff,ma13vdiff,
                     ma14vdiff,ma15vdiff,ma16vdiff,ma17vdiff,ma18vdiff,ma19vdiff,ma20vdiff,
                     ma21vdiff,ma22vdiff,ma23vdiff,ml1vdiff,ml2vdiff,ml3vdiff,ml4vdiff,ml5vdiff,ml6vdiff,ml7vdiff,ml8vdiff,ml9vdiff,
                     ml10vdiff,ml11vdiff,ml12vdiff,mh1vdiff,mh2vdiff,mh3vdiff,mh4vdiff,mh5vdiff,
                     mh6vdiff,mh7vdiff,mh8vdiff,mh9vdiff,mh10vdiff,mh11vdiff,mh12vdiff,l7Q10diff,l7Q2diff,return_10diff,pbiasv,comment)
colnames(statsout)<-c('site_no','nse','nselog','rmse','min_date','max_date','mean_of_annual_flows','median_of_annual_flows','cv_of_annual_flows',
                      'nse_90','nse_75_90','nse_50_75','nse_25_50','nse_10_25','nse_10',
                      'rmse_90','rmse_75_90','rmse_50_75','rmse_25_50','rmse_10_25','rmse_10',
                      'pbias_90','pbias_75_90','pbias_50_75','pbias_25_50','pbias_10_25','pbias_10',
                      'flow_10_obs,','flow_25_obs','flow_50_obs','flow_75_obs','flow_90_obs',
                      'cv_daily_flows','skew_daily_flows','ma12_jan_mean','ma13_feb_mean',
                      'ma14_mar_mean','ma15_apr_mean','ma16_may_mean','ma17_june_mean','ma18_july_mean','ma19_aug_mean','ma20_sep_mean',
                      'ma21_oct_mean','ma22_nov_mean','ma23_dec_mean','ml1','ml2','ml3','ml4','ml5','ml6','ml7','ml8','ml9',
                      'ml10','ml11','ml12',
                      'mh1','mh2','mh3','mh4','mh5','mh6','mh7','mh8','mh9','mh10','mh11','mh12','7Q10_obs','7Q2_obs','10_year_return_max_obs',
                      'mean_of_annual_flows_mod','median_of_annual_flows_mod','cv_of_annual_flows_mod','cv_daily_flows_mod','skew_daily_flows_mod',
                      'flow_10_mod_mod','flow_25_mod_mod','flow_50_mod_mod','flow_75_mod_mod','flow_90_mod_mod','ma12_jan_mean_mod','ma13_feb_mean_mod',
                      'ma14_mar_mean_mod','ma15_apr_mean_mod','ma16_may_mean_mod','ma17_june_mean_mod','ma18_july_mean_mod','ma19_aug_mean_mod','ma20_sep_mean_mod',
                      'ma21_oct_mean_mod','ma22_nov_mean_mod','ml1_mod','ml2_mod','ml3_mod','ml4_mod','ml5_mod','ml6_mod','ml7_mod','ml8_mod','ml9_mod',
                      'ml10_mod','ml11_mod','ml12_mod',
                      'mh1_mod','mh2_mod','mh3_mod','mh4_mod','mh5_mod','mh6_mod','mh7_mod','mh8_mod','mh9_mod','mh10_mod','mh11_mod','mh12_mod','7Q10_mod','7Q2_mod','10_year_return_max_mod','mean_flow_diff','med_flow_diff','cv_flow_diff','cv_daily_diff',
                      'skew_daily_diff','flow_10_diff','flow_25_diff','flow_50_diff','flow_75_diff','flow_90_diff',
                      'ma12_jan_mean_diff','ma13_feb_mean_diff',
                      'ma14_mar_mean_diff','ma15_apr_mean_diff','ma16_may_mean_diff','ma17_june_mean_diff','ma18_july_mean_diff','ma19_aug_mean_diff','ma20_sep_mean_diff',
                      'ma21_oct_mean_diff','ma22_nov_mean_diff','ma23_dec_mean_diff','ml1_diff','ml2_diff','ml3_diff','ml4_diff','ml5_diff','ml6_diff','ml7_diff','ml8_diff','ml9_diff',
                      'ml10_diff','ml11_diff','ml12_diff',
                      'mh1_diff','mh2_diff','mh3_diff','mh4_diff','mh5_diff','mh6_diff','mh7_diff','mh8_diff','mh9_diff','mh10_diff','mh11_diff','mh12_diff','7Q10_diff','7Q2_diff','10_year_return_max_diff','percent_bias','comment')
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
