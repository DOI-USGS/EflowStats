library(hydroGOF)
library(HITHATStats)
library(NWCCompare)

sos_url="http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering='00003'
property='00060'
drainage_url="http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
site_url="http://cida-wiwsc-gdp2qa.er.usgs.gov:8082/geoserver/nwc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=nwc:se_sites"

setwd('/Users/jlthomps/Documents/R/')
#a<-read.csv(header=F,colClasses=c("character"),text=sites)
a<-read.csv("sites_waters_stat_part.txt",header=F,colClasses=c("character"))
#a<-t(getAllSites(site_url))
system("rm monthly*txt")
al<-length(a)
comment<-vector(length=al)
ObsFlowStats <- matrix(nrow=al,ncol=137)
yv<-vector(length=al)
ymaxv<-vector(length=al)

for (i in 1:length(a)){
  startdate<-"1900-01-01"
  enddate<-"2012-10-01"
  sites=a[i]
  url2<-paste(sos_url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,'&access=3',sep='')
  x_obs <- getXMLWML1.1Data(url2)

  if (nrow(x_obs)>2) {
    obs_data <- get_obsdata(x_obs)
    drain_url<-paste(drainage_url,sites,sep="")
    drain_area<-getDrainageArea(drain_url)

    if (length(obs_data$discharge)<4) { 
      comment[i]<-"No complete water years of data available"
    } else {
      yv[i]<-as.character(min(obs_data$date))
      ymaxv[i]<-as.character(max(obs_data$date))
      x_obsz<-obs_data$discharge
      dates<-as.Date(obs_data$date)
      file<-paste("monthly_mean_ts_obs",toString(sites),".txt",sep="")
      monthly_mean<-monthly.mean.ts(obs_data)
      write.table(monthly_mean,file=file,col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

      obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
      ObsFlowStats[i,] <- FlowStats(obs_data,drain_area)
      comment <- ""
    }
    } else {
      comment[i]<-"No observed data for this site"
    }
}


statsout<-data.frame(a,yv,ymaxv,ObsFlowStats,comment)
#colnames(statsout)<-c('site_no','min_date','max_date','mean_of_annual_flows','median_of_annual_flows','cv_of_annual_flows',
#                      'flow_10_obs,','flow_25_obs','flow_50_obs','flow_75_obs','flow_90_obs',
#                      'cv_daily_flows','skew_daily_flows','ma1_mean_disc','ma2_median_disc','ma3_mean_annual_var','ma4','ma5_skew','ma6','ma7','ma8','ma9','ma10','ma11','ma12_jan_mean','ma13_feb_mean',
#                      'ma14_mar_mean','ma15_apr_mean','ma16_may_mean','ma17_june_mean','ma18_july_mean','ma19_aug_mean','ma20_sep_mean',
#                      'ma21_oct_mean','ma22_nov_mean','ma23_dec_mean','ma24_jan_var','ma25_feb_var','ma26_mar_var','ma27_apr_var',
#                      'ma28_may_var','ma29_jun_var','ma30_july_var','ma31_aug_var','ma32_sep_var','ma33_oct_var','ma34_nov_var','ma35_dec_var','ma36',
#                      'ma37_var_across_months','ma38','ma39_monthly_std_dev','ma40_monthly_skewness','ma41','ma42','ma43','ma44','ma45','ml1','ml2','ml3','ml4','ml5','ml6','ml7','ml8','ml9',
#                      'ml10','ml11','ml12','ml13_min_monthly_var','ml14_min_annual_flow','ml15','ml16','ml17','ml18','ml19','ml20','ml21','ml22',
#                      'mh1','mh2','mh3','mh4','mh5','mh6','mh7','mh8','mh9','mh10','mh11','mh12','mh13','mh14_med_annual_max',
#                      'mh15','mh16_high_flow_index','mh17','mh18','mh19','mh20','mh21','mh22','mh23','mh24','mh25','mh26_high_peak_flow','mh27',
#                      'fl1_low_flood_pulse','fl2_low_pulse_var','fh1_high_pulse_count','fh2_high_pulse_var','fh3_high_pulse_count_three',
#                      'fh4_high_pulse_count_seven','dl1_min_daily_flow','dl2_min_3_day_avg','dl3','dl4_min_30_day_avg','dl5_min_90_day_avg',
#                      'dl6_min_flow_var','dl7','dl8','dl9_min_30_day_var','dl10_min_90_day_var','dl18_zero_flow_days','dh1','dh2','dh3','dh4','dh5_max_90_day_avg',
#                      'dh10_max_90_day_var','dh11','tl1_min_flow_julian_day','tl2_min_julian_var','th1_max_flow_julian_day','th2_max_julian_var',
#                      'ra1_rise_rate','ra2_rise_rate_var','ra3_fall_rate','ra4_fall_rate_var','7Q10_obs','7Q2_obs','10_year_return_max_obs',
#                      'comment')
output="output.zip"
if (i==length(a)) {
  write.table(statsout,file="output.txt",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
  system("rm output.zip")
  system("zip -r output monthly*txt")
  system("zip -r output output*")
} else { 
  output="output.zip" 
  message<-"One or more web service calls resulted in failure. Please try again."
  write.table(message,file="output.txt",col.names=FALSE,row.names=FALSE,quote=FALSE)
}
