library(hydroGOF)
library(HITHATStats)
library(NWCCompare)

sos_url="http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering='00003'
property='00060'
drainage_url="http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="

startdate<-"1900-01-01"
enddate<-"2012-10-01"
#sites<-"05382257"
#url2<-paste(sos_url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,'&access=3',sep='')
#x_obs <- getXMLWML1.1Data(url2)

sites<-"02178400"
a<-sites
startdate<-"2008-10-01"
enddate<-"2013-9-30"
interval<-''
latest<-''
url2<-paste(sos_url_temp,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering_temp,'&parameterCd=',property_temp,sep='')
x_obs <- getXMLWML1.1Data(url2)
if (nrow(x_obs)>2) {
  data <- get_obsdata(x_obs)
  drain_url<-paste(drainage_url,sites,sep="")
  drain_area<-getDrainageArea(drain_url)
  countbyyr<-aggregate(data$discharge, list(data$wy_val), length)
  colnames(countbyyr)<-c('wy','num_samples')
  sub_countbyyr<-subset(countbyyr,num_samples >= 365)
  if (nrow(sub_countbyyr)==0) {
    comment[i]<-"No matching complete water years for site"
  } else {
    data<-merge(data,sub_countbyyr,by.x="wy_val",by.y="wy")
    yv<-as.character(min(data$date))
    ymaxv<-as.character(max(data$date))
    x_obsz<-data$discharge
    dates<-as.Date(data$date)
    file<-paste("monthly_mean_ts_obs",toString(sites),".txt",sep="")
    monthly_mean<-monthly.mean.ts(data)
    write.table(monthly_mean,file=file,col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

    data <- data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
    sdbyyr <- aggregate(data$discharge, list(data$year_val), 
                        sd)
    colnames(sdbyyr) <- c("Year", "sdq")
    meanbyyr <- aggregate(data$discharge, list(data$year_val), 
                          mean, na.rm=TRUE)
    colnames(meanbyyr) <- c("Year", "meanq")
    medbyyr <- aggregate(data$discharge, list(data$year_val), 
                         median, na.rm=TRUE)
    colnames(medbyyr) <- c("Year","medq")
    dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
                           meanbyyr$meanq, medbyyr$medq)
    colnames(dfcvbyyr) <- c("Year", "sdq", "meanq", "medq")
    cvbyyr <- dfcvbyyr$sdq/dfcvbyyr$meanq
    dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr)
    colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", "medq", 
                             "cvq")
    
    mean_flow<-mean(dfcvbyyrf$meanq,na.rm=TRUE)
    med_flow<-median(dfcvbyyrf$meanq,na.rm=TRUE)
    cv_flow<-sd(dfcvbyyrf$meanq,na.rm=TRUE)/mean(dfcvbyyrf$meanq,na.rm=TRUE)
    cv_daily<-cv(data)
    ma1v<-ma1(data)
    ma2v<-ma2(data)
    ma3v<-ma3(data)
    ma4v<-unlist(ma4.11(data)[1])
    ma5v<-unlist(ma4.11(data)[2])
    ma6v<-unlist(ma4.11(data)[3])
    ma7v<-unlist(ma4.11(data)[4])
    ma8v<-unlist(ma4.11(data)[5])
    ma9v<-unlist(ma4.11(data)[6])
    ma10v<-unlist(ma4.11(data)[7])
    ma11v<-unlist(ma4.11(data)[8])
    ma12v<-ma12.23(data)[1:1,2:2]
    ma13v<-ma12.23(data)[2:2,2:2]
    ma14v<-ma12.23(data)[3:3,2:2]
    ma15v<-ma12.23(data)[4:4,2:2]
    ma16v<-ma12.23(data)[5:5,2:2]
    ma17v<-ma12.23(data)[6:6,2:2]
    ma18v<-ma12.23(data)[7:7,2:2]
    ma19v<-ma12.23(data)[8:8,2:2]
    ma20v<-ma12.23(data)[9:9,2:2]
    ma21v<-ma12.23(data)[10:10,2:2]
    ma22v<-ma12.23(data)[11:11,2:2]
    ma23v<-ma12.23(data)[12:12,2:2]
    ma24v<-ma24.35(data)[1,1]
    ma25v<-ma24.35(data)[2,1]
    ma26v<-ma24.35(data)[3,1]
    ma27v<-ma24.35(data)[4,1]
    ma28v<-ma24.35(data)[5,1]
    ma29v<-ma24.35(data)[6,1]
    ma30v<-ma24.35(data)[7,1]
    ma31v<-ma24.35(data)[8,1]
    ma32v<-ma24.35(data)[9,1]
    ma33v<-ma24.35(data)[10,1]
    ma34v<-ma24.35(data)[11,1]
    ma35v<-ma24.35(data)[12,1]
    ma36v<-unlist(ma36.40(data)[1])
    ma37v<-unname(unlist(ma36.40(data)[2]))
    ma38v<-unname(unlist(ma36.40(data)[3]))
    ma39v<-unlist(ma36.40(data)[4])
    ma40v<-unlist(ma36.40(data)[5])
    ma41v<-unlist(ma41.45(data,drain_area)[1])
    ma42v<-unlist(ma41.45(data,drain_area)[2])
    ma43v<-unname(unlist(ma41.45(data,drain_area)[3]))
    ma44v<-unname(unlist(ma41.45(data,drain_area)[4]))
    ma45v<-unlist(ma41.45(data,drain_area)[5])
    ml1v<-unlist(ml1.12(data)[1])
    ml2v<-unlist(ml1.12(data)[2])
    ml3v<-unlist(ml1.12(data)[3])
    ml4v<-unlist(ml1.12(data)[4])
    ml5v<-unlist(ml1.12(data)[5])
    ml6v<-unlist(ml1.12(data)[6])
    ml7v<-unlist(ml1.12(data)[7])
    ml8v<-unlist(ml1.12(data)[8])
    ml9v<-unlist(ml1.12(data)[9])
    ml10v<-unlist(ml1.12(data)[10])
    ml11v<-unlist(ml1.12(data)[11])
    ml12v<-unlist(ml1.12(data)[12])
    ml13v<-ml13(data)
    ml14v<-unlist(ml14.16(data)[1])
    ml15v<-unlist(ml14.16(data)[2])
    ml16v<-unlist(ml14.16(data)[3])
    ml17v<-ml17(data)
    ml18v<-ml18(data)
    ml19v<-ml19(data)
    ml20v<-ml20(data)
    ml21v<-ml21(data)
    ml22v<-ml22(data,drain_area)
    mh1v<-unlist(mh1.12(data)[1])
    mh2v<-unlist(mh1.12(data)[2])
    mh3v<-unlist(mh1.12(data)[3])
    mh4v<-unlist(mh1.12(data)[4])
    mh5v<-unlist(mh1.12(data)[5])
    mh6v<-unlist(mh1.12(data)[6])
    mh7v<-unlist(mh1.12(data)[7])
    mh8v<-unlist(mh1.12(data)[8])
    mh9v<-unlist(mh1.12(data)[9])
    mh10v<-unlist(mh1.12(data)[10])
    mh11v<-unlist(mh1.12(data)[11])
    mh12v<-unlist(mh1.12(data)[12])
    mh13v<-mh13(data)
    mh14v<-mh14(data)
    mh15v<-unname(unlist(mh15.17(data)[1]))
    mh16v<-unname(unlist(mh15.17(data)[2]))
    mh17v<-unname(unlist(mh15.17(data)[3]))
    mh18v<-mh18(data)
    mh19v<-mh19(data)
    mh20v<-mh20(data,drain_area)
    mh21v<-mh21(data)
    mh22v<-mh22(data)
    mh23v<-mh23(data)
    mh24v<-mh24(data)
    mh25v<-mh25(data)
    mh26v<-mh26(data)
    mh27v<-mh27(data)
    fl1v<-unname(unlist(fl1.2(data)[1]))
    fl2v<-unname(unlist(fl1.2(data)[2]))
    fl3v<-fl3(data)
    fh1v<-unname(unlist(fh1.2(data)[1]))
    fh2v<-unname(unlist(fh1.2(data)[2]))
    fh3v<-fh3(data) 
    fh4v<-fh4(data) 
    fh5v<-fh5(data)
    fh6v<-fh6(data)
    fh7v<-fh7(data)
    fh8v<-fh8(data)
    fh9v<-fh9(data)
    fh10v<-fh10(data)
    #peakValues<-getPeakData(sites)
    #thresh_60<-getPeakThresh(data,qfilepeak,.6)
    #thresh_40<-getPeakThresh(data,qfilepeak,.4)
    #fh11v<-fh11(data,thresh_60)
    dl1v<-dl1(data)
    dl2v<-dl2(data)
    dl3v<-dl3(data)
    dl4v<-dl4(data)
    dl5v<-dl5(data)
    dl6v<-dl6(data)
    dl7v<-dl7(data)
    dl8v<-dl8(data)
    dl9v<-dl9(data)
    dl10v<-dl10(data)
    dl11v<-dl11(data)
    dl12v<-dl12(data)
    dl13v<-dl13(data)
    dl14v<-unname(dl14(data))
    dl15v<-unname(dl15(data))
    dl16v<-unname(unlist(dl16.17(data)[1]))
    dl17v<-unname(unlist(dl16.17(data)[2]))
    dl18v<-dl18(data)
    dl19v<-dl19(data)
    dl20v<-dl20(data)
    dh1v<-dh1(data)
    dh2v<-dh2(data)
    dh3v<-dh3(data)
    dh4v<-dh4(data)
    dh5v<-dh5(data)
    dh6v<-dh6(data)
    dh7v<-dh7(data)
    dh8v<-dh8(data)
    dh9v<-dh9(data)
    dh10v<-dh10(data)
    dh11v<-dh11(data)
    dh12v<-dh12(data)
    dh13v<-dh13(data)
    dh14v<-unname(dh14(data))
    dh15v<-unname(unlist(dh15.16(data)[1]))
    dh16v<-unname(unlist(dh15.16(data)[2]))
    dh17v<-dh17(data)
    dh18v<-dh18(data)
    dh19v<-dh19(data)
    dh20v<-dh20(data)
    dh21v<-dh21(data)
    #dh22v<-dh22(data,thresh_60)
    #dh23v<-dh23(data,thresh_60)
    #dh24v<-dh24(data,thresh_60)
    ta1v<-unname(unlist(ta1.2(data)[1]))
    ta2v<-unname(unlist(ta1.2(data)[2]))
    #ta3v<-ta3(data,thresh_60)
    tl1v<-unname(unlist(tl1.2(data)[1]))
    tl2v<-unname(unlist(tl1.2(data)[2]))
    #tl3v<-tl3(data,thresh_40)
    #tl4v<-tl4(data,thresh_40)
    th1v<-unname(unlist(th1.2(data)[1]))
    th2v<-unname(unlist(th1.2(data)[2]))
    #th3v<-th3(data,thresh_60)
    ra1v<-ra1(data)
    ra2v<-ra2(data)
    ra3v<-ra3(data)
    ra4v<-ra4(data)
    ra5v<-ra5(data)
    ra6v<-ra6(data)
    ra7v<-ra7(data)
    ra8v<-unname(unlist(ra8.9(data)[1]))
    ra9v<-unname(unlist(ra8.9(data)[2]))
    l7Q10v<-l7Q10(data)
    l7Q2v<-l7Q2(data)
    return_10v<-return_10(data)
    
    obs_percentiles <- quantile(data$discharge,probs=c(0.10, 0.25, 0.50, 0.75, 0.90, 0.15),na.rm=TRUE)
    flow_10 <- unname(obs_percentiles[1])
    flow_25 <- unname(obs_percentiles[2])
    flow_50 <- unname(obs_percentiles[3])
    flow_75 <- unname(obs_percentiles[4])
    flow_90 <- unname(obs_percentiles[5])
    flow_15 <- unname(obs_percentiles[6])
    comment <- ""
  }
  } else {
      comment<-"No observed data for this site"
    }

statsout<-data.frame(sites,yv,ymaxv,mean_flow,med_flow,cv_flow,cv_daily,ma1v,ma2v,ma3v,ma4v,ma5v,ma6v,ma7v,ma8v,ma9v,ma10v,ma11v,ma12v,
                     ma13v,ma14v,ma15v,ma16v,ma17v,ma18v,ma19v,ma20v,ma21v,ma22v,ma23v,ma24v,ma25v,ma26v,ma27v,ma28v,
                     ma29v,ma30v,ma31v,ma32v,ma33v,ma34v,ma35v,ma36v,ma37v,ma38v,ma39v,ma40v,ma41v,ma42v,ma43v,ma44v,
                     ma45v,ml1v,ml2v,ml3v,ml4v,ml5v,ml6v,ml7v,ml8v,ml9v,ml10v,ml11v,ml12v,ml13v,ml14v,ml15v,ml16v,
                     ml17v,ml18v,ml19v,ml20v,ml21v,ml22v,mh1v,mh2v,mh3v,mh4v,mh5v,mh6v,mh7v,mh8v,mh9v,mh10v,mh11v,
                     mh12v,mh13v,mh14v,mh15v,mh16v,mh17v,mh18v,mh19v,mh20v,mh21v,mh22v,mh23v,mh24v,mh25v,mh26v,mh27v,
                     fl1v,fl2v,fl3v,fh1v,fh2v,fh3v,fh4v,fh5v,fh6v,fh7v,fh8v,fh9v,fh10v,dl1v,dl2v,dl3v,dl4v,
                     dl5v,dl6v,dl7v,dl8v,dl9v,dl10v,dl11v,dl12v,dl13v,dl14v,dl15v,dl16v,dl17v,dl18v,dl19v,dl20v,
                     dh1v,dh2v,dh3v,dh4v,dh5v,dh6v,dh7v,dh8v,dh9v,dh10v,dh11v,dh12v,dh13v,dh14v,dh15v,
                     dh16v,dh17v,dh18v,dh19v,dh20v,dh21v,ta1v,ta2v,tl1v,tl2v,
                     th1v,th2v,ra1v,ra2v,ra3v,ra4v,ra5v,ra6v,ra7v,ra8v,ra9v,l7Q10v,l7Q2v,return_10v,
                     flow_10,flow_25,flow_50,flow_75,flow_90,flow_15,comment)
colnames(statsout)<-c('site_no','min_date','max_date','mean_of_annual_flows','median_of_annual_flows','cv_of_annual_flows',
                      'cv_daily_flows','ma1_mean_disc','ma2_median_disc','ma3_mean_annual_var','ma4','ma5_skew','ma6','ma7','ma8','ma9','ma10','ma11','ma12_jan_mean','ma13_feb_mean',
                      'ma14_mar_mean','ma15_apr_mean','ma16_may_mean','ma17_june_mean','ma18_july_mean','ma19_aug_mean','ma20_sep_mean',
                      'ma21_oct_mean','ma22_nov_mean','ma23_dec_mean','ma24_jan_var','ma25_feb_var','ma26_mar_var','ma27_apr_var',
                      'ma28_may_var','ma29_jun_var','ma30_july_var','ma31_aug_var','ma32_sep_var','ma33_oct_var','ma34_nov_var','ma35_dec_var','ma36',
                      'ma37_var_across_months','ma38','ma39_monthly_std_dev','ma40_monthly_skewness','ma41','ma42','ma43','ma44','ma45','ml1','ml2','ml3','ml4','ml5','ml6','ml7','ml8','ml9',
                      'ml10','ml11','ml12','ml13_min_monthly_var','ml14_min_annual_flow','ml15','ml16','ml17','ml18','ml19','ml20','ml21','ml22',
                      'mh1','mh2','mh3','mh4','mh5','mh6','mh7','mh8','mh9','mh10','mh11','mh12','mh13','mh14_med_annual_max',
                      'mh15','mh16_high_flow_index','mh17','mh18','mh19','mh20','mh21','mh22','mh23','mh24','mh25','mh26_high_peak_flow','mh27',
                      'fl1_low_flood_pulse','fl2_low_pulse_var','fl3','fh1_high_pulse_count','fh2_high_pulse_var','fh3_high_pulse_count_three',
                      'fh4_high_pulse_count_seven','fh5','fh6','fh7','fh8','fh9','fh10','dl1_min_daily_flow','dl2_min_3_day_avg','dl3','dl4_min_30_day_avg','dl5_min_90_day_avg',
                      'dl6_min_flow_var','dl7','dl8','dl9_min_30_day_var','dl10_min_90_day_var','dl11','dl12','dl13','dl14','dl15','dl16','dl17','dl18_zero_flow_days','dl19','dl20',
                      'dh1','dh2','dh3','dh4','dh5_max_90_day_avg','dh6','dh7','dh8','dh9',
                      'dh10_max_90_day_var','dh11','dh12','dh13','dh14','dh15','dh16','dh17','dh18','dh19','dh20','dh21',
                      'ta1','ta2','tl1_min_flow_julian_day','tl2_min_julian_var','th1_max_flow_julian_day','th2_max_julian_var',
                      'ra1_rise_rate','ra2_rise_rate_var','ra3_fall_rate','ra4_fall_rate_var','ra5','ra6','ra7','ra8','ra9','7Q10_obs','7Q2_obs','10_year_return_max_obs',
                      'flow_10','flow_25','flow_50','flow_75','flow_90','flow_15','comment')
output="output.txt"
write.table(statsout,file="output.txt",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
