# This program contains functions to compute selected streamflow statistics
# from the larger set of flow statistics listed in Olden and Poff, 2003. 
# Included in this data set are selected IHA statistics as well. The functions 
# are named after the flow statistic ID in Olden and Poff, 2003. 
# (Created by S.A. Archfield on November 13, 2010)

# The user has the option to choose median or mean values. The default is 
# mean values. 

# For this version (compflowstats.r), all functions listed below require 
# a dataframe with column names p99_00060_00003, month_val, year_val and must
# first be run through the flow.data function to create the data columns used
# in the stats computations.

# source('C:/Data/daily flow estimation/statscalcs/sarchfield/compflowstats.r')
# modified JEK 2/21/2012
# changed outputs slightly - added station ID, changed to tab delimited, no quotes

library(zoo)
library(chron)
library(doBy)

# This function creates additional variables to the original file to be used
# in the calculation of the flow statistics. This function assumes that the
# input file has column named 'datetime' with the date formatted as YYYYMMDD
# and flow is located in a column named 'p99_00060_00003'. Other format 
# functions can be developed to accodate other input formats. 
flow.data<-function(qfiletemp,incremnt,syr,eyr){ 
 	#Query time series for subset of length equal to the increment
 	selqfile<-subset(qfiletemp,qfiletemp$datetime>=syr&qfiletemp$datetime<=eyr)
 	tempdatafr<-NULL
 	tempdatafr<-data.frame(selqfile)
  #Create additional variables for each day
  month_val<-rep(0,length(tempdatafr$datetime))
  year_val<-rep(0,length(tempdatafr$datetime))
  day_val<-rep(0,length(tempdatafr$datetime))
  jul_val<-rep(0,length(tempdatafr$datetime))
  #Add additonal variables to the dataframe
  qfiletempf<<-data.frame(tempdatafr$datetime,tempdatafr$p99_00060_00003,month_val,year_val,day_val,jul_val)
  colnames(qfiletempf)<<-c('datetime','p99_00060_00003','month_val','year_val','day_val','jul_val')
  #Compute the additional variables to the dataframe
  qfiletempf$month_val<<-substr(qfiletempf$datetime,4,5)
  as.numeric(qfiletempf$month_val)
  qfiletempf$year_val<<-substr(qfiletempf$datetime,1,2)
  as.numeric(qfiletempf$year_val)
  qfiletempf$day_val<<-substr(qfiletempf$datetime,7,8)
  as.numeric(qfiletempf$day_val)
  qfiletempf$jul_val<<-strptime(qfiletempf$datetime, "%Y%m%d")$yday+1
} 

flow.data1630<-function(qfiletemp){ 
 	#Query time series for subset of length equal to the increment
 	colnames(qfiletemp)<-c('datetime','p99_00060_00003')
 	#qfiletemp$datetime<-chron(qfiletemp$datetime,format=c(dates="y-m-d"),origin=c(month=1,day=1,year=1990))
 	tempdatafr<-NULL
 	tempdatafr<-data.frame(qfiletemp)
  #Create additional variables for each day
  month_val<-rep(0,length(tempdatafr$datetime))
  year_val<-rep(0,length(tempdatafr$datetime))
  day_val<-rep(0,length(tempdatafr$datetime))
  jul_val<-rep(0,length(tempdatafr$datetime))
  #Add additonal variables to the dataframe
  qfiletempf<-data.frame(tempdatafr$datetime,tempdatafr$p99_00060_00003,month_val,year_val,day_val,jul_val)
  colnames(qfiletempf)<-c('datetime','p99_00060_00003','month_val','year_val','day_val','jul_val')
  #Compute the additional variables to the dataframe
  qfiletempf$month_val<-as.numeric(months(qfiletemp$datetime)) #substr(qfiletempf$datetime,6,7)
  #qfiletempf$month_val<-as.numeric(qfiletempf$month_val)
  qfiletempf$year_val<-paste(substr(qfiletempf$datetime,1,4))
  qfiletempf$year_val<-as.numeric(qfiletempf$year_val)
  qfiletempf$day_val<-substr(qfiletempf$datetime,9,10)
  qfiletempf$day_val<-as.numeric(days(qfiletemp$datetime))
  qfiletempf$jul_val<-strptime(qfiletempf$datetime, "%Y-%B-%d")$yday+1
  flow.data1630<-qfiletempf
}                                                       

# Mean of the daily mean flow values for the entire flow record 
# (cubic feet per second – temporal).
ma1<-function(datafile){ma1<-mean(datafile$p99_00060_00003)}

# Median of the daily mean flow values for the entire flow record 
# (cubic feet per second – temporal).
ma2<-function(datafile){ma2<-median(datafile$p99_00060_00003)}

# The skewness of the entire flow record is computed as the mean for 
# the entire flow record (MA1) divided by the median (MA2) for the 
# entire flow record (dimensionless - spatial).
ma5<-function(datafile){x1<-ma1(datafile); x2<-ma2(datafile); ma5<-x1/x2}

# Means (or medians - Use Preference option) of monthly flow values.
# For example, MA12 is the mean of all January flow values over the 
# entire record (cubic feet per second – temporal)
ma16.23<-function(datafile,pref='mean'){
    if(pref=='median') {
      medmon<-aggregate(datafile$p99_00060_00003, list(datafile$month_val), median)
      ma16.23<-data.frame(medmon[2])}
    else {
      meanmon<-aggregate(datafile$p99_00060_00003, list(datafile$month_val), mean)
      ma16.23<-data.frame(meanmon[2])}
    #for specific month, type ma16.23[monthnumber,1]
}

# Variability (coefficient of variation) of monthly flow values. Compute the
# MA35 standard deviation for each month in each year over the entire flow 
# record. Divide the standard deviation by the mean for each month. Average 
# (or median - Use Preference option) these values for each month across 
# all years (percent – temporal)
ma24.35<-function(datafile,pref='mean'){
    #Compute standard deviation of the monthly flows by year
    sdmonbyyr<-aggregate(datafile$p99_00060_00003, list(datafile$year_val,datafile$month_val), FUN=sd)
    colnames(sdmonbyyr)<-c("Year","Month","sdq")
    #Compute mean of the monthly flows by year
    meanmonbyyr<-aggregate(datafile$p99_00060_00003, list(datafile$year_val,datafile$month_val), FUN=mean)
    colnames(meanmonbyyr)<-c("Year","Month","meanq")                
    # Make dataframe of mean and sd to compute coefficient of variation
    dfcvmonbyyr<-data.frame(meanmonbyyr$Year, meanmonbyyr$Month, sdmonbyyr$sdq, meanmonbyyr$meanq)
    colnames(dfcvmonbyyr)<-c("Year","Month","sdq","meanq")
    cvmonbyyr<-dfcvmonbyyr$sdq/dfcvmonbyyr$meanq
    dfcvmonbyyrf<-data.frame(dfcvmonbyyr,cvmonbyyr)
    colnames(dfcvmonbyyrf)<-c("Year","Month","sdq","meanq","cvq")
    #Compute the coefficient of variation of the monthly flows by year
    if(pref=='median') {
      medmoncv<-aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), median)
      ma24.35<-data.frame(medmoncv[2])}
    else {
      meanmoncv<-aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), mean)
      ma24.35<-data.frame(meanmoncv[2])}
}

# Variability across monthly flows. Compute the first (25th percentile) and the 
# third (75th percentile) quartiles (every month in the flow record). MA37 is 
# the third quartile minus the first quartile divided by the median of the 
# monthly means (dimensionless – spatial).
ma37<-function(datafile){
    #Compute mean flow by month and year
    sdmonbyyr<-aggregate(datafile$p99_00060_00003, list(datafile$year_val,datafile$month_val), FUN=mean)
    colnames(sdmonbyyr)<-c("Year","Month","meanmo")
    #Find quartiles
    quart<-summary(sdmonbyyr$meanmo)
    thirdquart<-quart[5]
    firstquart<-quart[2]
    #Find difference between quartiles
    diffquart<-thirdquart-firstquart
    #Find mean of monthly flows
    medianquart<-quart[3]
    #Find difference divided by median
    ma37<-diffquart[1]/medianquart[1]
}

# Variability across monthly flows. Compute the standard deviation for the 
# monthly means. MA39 is the standard deviation times 100 divided by the mean 
# of the monthly means (percent – spatial).
ma39<-function(datafile){
    #Compute mean flow by month and year
    sdmonbyyr<-aggregate(datafile$p99_00060_00003, list(datafile$year_val,datafile$month_val), FUN=mean)
    colnames(sdmonbyyr)<-c("Year","Month","meanmo")    
    #Compute standard deviation of monthly flows
    sdmonflows<-sd(sdmonbyyr$meanmo)
    #Compute the mean of the monthly flows
    meanmonflows<-mean(sdmonbyyr$meanmo)
    ma39<-(sdmonflows*100)/meanmonflows
}

# Skewness in the monthly flows. MA40 is the mean of the monthly flow means 
# minus the median of the monthly means divided by the median of the monthly 
# means (dimensionless – spatial).
ma40<-function(datafile){
    numer1<-mean(ma16.23(datafile,pref='mean'))
    numer2<-median(ma16.23(datafile,pref='mean'))
    ma40<-(numer1-numer2)/numer2      
}

# Annual runoff. Compute the annual mean daily flows. MA41 is the mean of the 
# annual means divided by the drainage area (cubic feet per second/square mile 
# – temporal).
ma41<-function(datafile,darea){


}

# Variability (coefficient of variation) across minimum monthly flow values. 
# Compute the mean and standard deviation for the minimum monthly flows over 
# the entire flow record. ML13 is the standard deviation times 100 divided by 
# the mean minimum monthly flow for all years (percent – spatial).
ml13<-function(datafile){
    #Compute mean flow by month and year
    minmonbyyr<-aggregate(datafile$p99_00060_00003, list(datafile$year_val,datafile$month_val), FUN=min)
    colnames(minmonbyyr)<-c("Year","Month","minmo")
    #Compute standard deviation of min monthly flows
    sdminmonflows<-sd(minmonbyyr$minmo)
    #Compute the mean of the min monthly flows
    meanminmonflows<-mean(minmonbyyr$minmo)
    ml13<-(sdminmonflows*100)/meanminmonflows
}
# Compute the minimum annual flow for each year. ML14 is the mean of the 
# ratios of minimum annual flows to the median flow for each year 
# (dimensionless – temporal).
ml14<-function(datafile){
    #Compute 1-day mean minimum flow for each year
    min1daybyyear<-aggregate(datafile$p99_00060_00003,list(datafile$year_val),min)
    #Compute median annual flow for each year
    medflow<-medflowbyyear(datafile)
    #Divide 1-day mean minimum flow by the median annual flow for each year
    computeml14<-merge(min1daybyyear,medflow,by="Group.1")
    colnames(computeml14)<-c("year","day1min","meddaily")
    dfml14<-computeml14$day1min/computeml14$meddaily
    #Compute mean of annual minimum
    ml14<-mean(dfml14)
}

ml15<-function(datafile){
    #Compute 1-day mean minimum flow for each year
    min1daybyyear<-aggregate(datafile$p99_00060_00003,list(datafile$year_val),min)
    #Compute median annual flow for each year
    medflow<-medflowbyyear(datafile)
    #Divide 1-day mean minimum flow by the median annual flow for each year
    computeml14<-merge(min1daybyyear,medflow,by="Group.1")
    colnames(computeml14)<-c("year","day1min","meddaily")
    dfml14<-computeml14$day1min/computeml14$meddaily
    #Compute mean of annual minimum
    ml14<-mean(dfml14)
}

# Base flow. Compute the mean annual flows. Compute the minimum of a 
# 7-day moving average flow for each year and divide them by the mean 
# annual flow for that year. ML17 is the mean (or median - Use Preference 
# option) of those ratios (dimensionless – temporal).
ml17<-function(datafile,pref='mean'){
    #Use the function bfi to compute the base-flow index for each year
    bfibyyear<-bfi(datafile)
    #Compute the coefficient of variation of the monthly flows by year
    if(pref=='median') {ml17<-median(bfibyyear)} else {ml17<-mean(bfibyyear)}
}

# Variability in base flow. Compute the standard deviation for the ratios of 
# 7-day moving average flows to mean annual flows for each year. ML18 is the 
# standard deviation times 100 divided by the mean of the ratios 
# (percent – spatial).
ml18<-function(datafile){
    #Use the function bfi to compute the base-flow index for each year
    bfibyyear<-bfi(datafile)
    #Compute the standard deviation of the base-flow index
    sdbfi<-sd(bfibyyear)
    meanbfi<-mean(bfibyyear)
    #Compute the coefficient of variation of the base-flow index
    ml18<-(sdbfi*100)/meanbfi
} 

# Median of annual maximum flows. Compute the annual maximum flows from monthly 
# maximum flows. Compute the ratio of annual maximum flow to median annual flow 
# for each year. MH14 is the median of these ratios (dimensionless – temporal).
mh14<-function(datafile){
    #Compute maximum flow by year and month
    maxmonbymoyr<-aggregate(datafile$p99_00060_00003, list(datafile$year_val,datafile$month_val), FUN=max)
    colnames(maxmonbymoyr)<-c("Year","Month","momax")
    #Compute the maximum of the monthly flows by year
    maxmonbyyrr<-aggregate(maxmonbymoyr$momax, list(maxmonbymoyr$Year), FUN=max)
    colnames(maxmonbyyrr)<-c("Year","yrmax")
    #Compute the median flow for each year
    medflowbyyr<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), FUN=median)
    colnames(medflowbyyr)<-c("Year","yrmed")
    #Divide the maximum of the monthly flows by year by the median flow for each year
    ratiomaxmed<-maxmonbyyrr$yrmax/medflowbyyr$yrmed
    #Compute the median of ratiomaxmed
    mh14<-median(ratiomaxmed)   
}

# High flow discharge index. Compute the 10 percent exceedence value for the 
# entire data record. MH16 is the 10 percent exceedence value divided by the 
# median flow for the entire record (dimensionless – spatial).
mh16<-function(datafile){
    #Isolate the daily streamflow values    
    isolateq<-datafile$p99_00060_00003
    #Sort the daily streamflow values
    sortq<-sort(isolateq)
    #Find rank value in sortq that equals the 10-percent exceedence value
    frank<-floor(findrank(length(sortq),0.10))
    #Find streamflow value corresponding to the rank
    hfcrit<-sortq[frank]
    #Divide hfcrit by MA2 (median flow)
    mh16<-hfcrit/ma2(datafile)
}

# High peak flow. Compute the average peak flow value for flow events above 
# a threshold equal to seven times the median flow for the entire record. 
# MH26 is the average peak flow divided by the median flow for the entire 
# record (dimensionless – temporal).
mh26<-function(datafile){
    #Compute threshold (7*MA2)
    hfcrit<-7*ma2(datafile)
    #Isolate the daily streamflow values    
    isolateq<-datafile$p99_00060_00003
    #Query flow record for all events greater than hfcrit
    exchfcrit<-subset(isolateq,isolateq>hfcrit)
    #Take mean of streamflow values that exceed hfcrit
    meanex<-mean(exchfcrit)
    #Divide meanex by MA2
    mh26<-meanex/ma2(datafile)
}

# Low flood pulse count. Compute the average number of flow events with flows 
# below a threshold equal to the 25th percentile value for the entire flow record. 
# FL1 is the average (or median - Use Preference option) number of events 
# (number of events/year – temporal).
fl1<-function(datafile,pref='mean'){
    #Isolate the daily streamflow values    
    isolateq<-datafile$p99_00060_00003
    #Sort the daily streamflow values
    sortq<-sort(isolateq)
    #Find rank value in sortq that equals the 10-percent exceedence value
    frank<-floor(findrank(length(sortq),0.75))
    #Find streamflow value corresponding to the rank
    lfcrit<-sortq[frank]
    #For each year, subset data and count number of days flow exceeds threshold
    #Get number of years in data
    noyears<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    lfcountbyyr<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
        #print(i)
        subsetyr<-subset(datafile,datafile$year_val==i)
        echfcrit<-subset(subsetyr,subsetyr$p99_00060_00003<lfcrit)
        counter<-counter+1
        lfcountbyyr[counter]<-length(echfcrit$p99_00060_00003)
    }
    lfcntbyyr<<-lfcountbyyr
    if(pref=='median') {fl1<-median(lfcntbyyr)} else {fl1<-mean(lfcntbyyr)}
}

# Variability in low pulse count. Compute the standard deviation in the annual 
# pulse counts for FL1. FL2 is 100 times the standard deviation divided by the 
# mean pulse count (percent – spatial).
fl2<-function(datafile){
    #Call function for fl1 to generate hfcountbyyr
    meanfl1<-fl1(datafile,pref='mean')    
    #Compute stdev of hfcountbyyr
    stdevfl1<-sd(lfcntbyyr)
    fl2<-(stdevfl1*100)/meanfl1
} 

# High flood pulse count. Compute the average number of flow events with flows 
# above a threshold equal to the 75th percentile value for the entire flow record. 
# FH1 is the average (or median - Use Preference option) number of events 
# (number of events/year – temporal).
fh1<-function(datafile,pref='mean'){
    #Isolate the daily streamflow values    
    isolateq<-datafile$p99_00060_00003
    #Sort the daily streamflow values
    sortq<-sort(isolateq)
    #Find rank value in sortq that equals the 10-percent exceedence value
    frank<-floor(findrank(length(sortq),0.25))
    #Find streamflow value corresponding to the rank
    hfcrit<-sortq[frank]
    #For each year, subset data and count number of days flow exceeds threshold
    #Get number of years in data
    noyears<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    hfcountbyyr<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
        #print(i)
        subsetyr<-subset(datafile,datafile$year_val==i)
        echfcrit<-subset(subsetyr,subsetyr$p99_00060_00003>hfcrit)
        counter<-counter+1
        hfcountbyyr[counter]<-length(echfcrit$p99_00060_00003)
    }
    hfcntbyyr<<-hfcountbyyr
    if(pref=='median') {fh1<-median(hfcntbyyr)} else {fh1<-mean(hfcntbyyr)}
}

# Variability in low pulse count. Compute the standard deviation in the annual 
# pulse counts for FL1. FL2 is 100 times the standard deviation divided by the 
# mean pulse count (percent – spatial).
fh2<-function(datafile){
    #Call function for fl1 to generate hfcountbyyr
    meanfh1<-fh1(datafile,pref='mean')    
    #Compute stdev of hfcountbyyr
    stdevfl1<-sd(hfcntbyyr)
    fh2<-(stdevfl1*100)/meanfh1
}

# High flood pulse count. Compute the average number of days per year that the 
# flow is above a threshold equal to three times the median flow for the entire 
# record. FH3 is the mean (or median – Use Preference option) of the annual 
# number of days for all years (number of days/year – temporal).
fh3<-function(datafile,pref='mean'){
    #Compute threshold (3*MA2)
    hfcrit<-3*ma2(datafile)
    #For each year, subset data and count number of days flow exceeds threshold
    #Get number of years in data
    noyears<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    hfcountbyyrfh3<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
        #print(i)
        subsetyr<-subset(datafile,datafile$year_val==i)
        echfcrit<-subset(subsetyr,subsetyr$p99_00060_00003>hfcrit)
        counter<-counter+1
        hfcountbyyrfh3[counter]<-length(echfcrit$p99_00060_00003)
    }
    hfcntbyyrfh3<<-hfcountbyyrfh3
    if(pref=='median') {fh3<-median(hfcntbyyrfh3)} else {fh3<-mean(hfcntbyyrfh3)}
}

# High flood pulse count. Compute the average number of days per year that the 
# flow is above a threshold equal to seven times the median flow for the entire 
# record. FH4 is the mean (or median - Use Preference option) of the annual 
# number of days for all years (number of days/year – temporal).
fh4<-function(datafile,pref='mean'){
    #Compute threshold (3*MA2)
    hfcrit<-7*ma2(datafile)
    #For each year, subset data and count number of days flow exceeds threshold
    #Get number of years in data
    noyears<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    hfcountbyyrfh4<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
        #print(i)
        subsetyr<-subset(datafile,datafile$year_val==i)
        echfcrit<-subset(subsetyr,subsetyr$p99_00060_00003>hfcrit)
        counter<-counter+1
        hfcountbyyrfh4[counter]<-length(echfcrit$p99_00060_00003)
    }
    hfcntbyyrfh4<<-hfcountbyyrfh4
    if(pref=='median') {fh4<-median(hfcntbyyrfh4)} else {fh4<-mean(hfcntbyyrfh4)}
}

# Annual minimum daily flow. Compute the minimum 1-day average flow for each 
# year. DL1 is the mean (or median - Use Preference option) of these values 
# (cubic feet per second – temporal).
dl1<-function(datafile,pref='mean'){
    #Compute the 1-day rolling mean
    day1mean<-rollmean(datafile$p99_00060_00003,1,align="right",na.pad=TRUE)
    #Add the day1mean series to the dataframe
    day1rollingavg<-data.frame(datafile,day1mean)
    #Subset rollongavgs1 so that there are no NA values
    rollingavgs1day<-subset(day1rollingavg,day1rollingavg$day1mean!="NA")
    #Compute annual minimum flow for the 1-day rolling mean for each year
    min1daybyyear<<-aggregate(rollingavgs1day$day1mean,list(rollingavgs1day$year_val),min)
    if(pref=='median') {dl1<-median(min1daybyyear$x)} else {dl1<-mean(min1daybyyear$x)}
}

# Annual minimum of 3-day moving average flow. Compute the minimum of a 3-day 
# moving average flow for each year. DL2 is the mean (or median - Use Preference 
# option) of these values (cubic feet per second – temporal).
dl2<-function(datafile,pref='mean'){
    #Compute the 3-day rolling mean
    day3mean<-rollmean(datafile$p99_00060_00003,3,align="right",na.pad=TRUE)
    #Add the day3mean series to the dataframe
    day3rollingavg<-data.frame(datafile,day3mean)
    #Subset rollongavgs3 so that there are no NA values
    rollingavgs3day<-subset(day3rollingavg,day3rollingavg$day3mean!="NA")
    #Compute annual minimum flow for the 3-day rolling mean for each year
    min3daybyyear<<-aggregate(rollingavgs3day$day3mean,list(rollingavgs3day$year_val),min)
    if(pref=='median') {dl2<-median(min3daybyyear$x)} else {dl2<-mean(min3daybyyear$x)}
}

# Annual minimum of 30-day moving average flow. Compute the minimum of a 30-day 
# moving average flow for each year. DL4 is the mean (or median - Use Preference 
# option) of these values (cubic feet per second – temporal).
dl4<-function(datafile,pref='mean'){
    #Compute the 30-day rolling mean
    day30mean<-rollmean(datafile$p99_00060_00003,30,align="right",na.pad=TRUE)
    #Add the day30mean series to the dataframe
    day30rollingavg<-data.frame(datafile,day30mean)
    #Subset rollongavgs30 so that there are no NA values
    rollingavgs30day<-subset(day30rollingavg,day30rollingavg$day30mean!="NA")
    #Compute annual minimum flow for the 30-day rolling mean for each year
    min30daybyyear<<-aggregate(rollingavgs30day$day30mean,list(rollingavgs30day$year_val),min)
    if(pref=='median') {dl5<-median(min30daybyyear$x)} else {dl5<-mean(min30daybyyear$x)}
}


# Annual minimum of 90-day moving average flow. Compute the minimum of a 90-day 
# moving average flow for each year. DL5 is the mean (or median - Use Preference 
# option) of these values (cubic feet per second – temporal).
dl5<-function(datafile,pref='mean'){
    #Compute the 90-day rolling mean
    day90mean<-rollmean(datafile$p99_00060_00003,90,align="right",na.pad=TRUE)
    #Add the day90mean series to the dataframe
    day90rollingavg<-data.frame(datafile,day90mean)
    #Subset rollongavgs90 so that there are no NA values
    rollingavgs90day<-subset(day90rollingavg,day90rollingavg$day90mean!="NA")
    #Compute annual minimum flow for the 90-day rolling mean for each year
    min90daybyyear<<-aggregate(rollingavgs90day$day90mean,list(rollingavgs90day$year_val),min)
    if(pref=='median') {dl5<-median(min90daybyyear$x)} else {dl5<-mean(min90daybyyear$x)}
}

# Variability of annual minimum daily average flow. Compute the standard 
# deviation for the minimum daily average flow. DL6 is 100 times the standard 
# deviation divided by the mean (percent – spatial).
dl6<-function(datafile){
    #Compute mean of dl1
    meandl6<-dl1(datafile,pref='mean')
    #Compute standard deviation of dl1 values by year
    sddl6<-sd(min1daybyyear$x)
    dl6<-(sddl6*100)/meandl6
}

# Variability of annual minimum of 30-day moving average flow. Compute the 
# standard deviation for the minimum 30-day moving averages. DL9 is 100 times 
# the standard deviation divided by the mean (percent - spatial).
dl9<-function(datafile){
    #Compute mean of dl4
    meandl9<-dl4(datafile,pref='mean')
    #Compute standard deviation of dl4 values by year
    sddl9<-sd(min30daybyyear$x)
    dl9<-(sddl9*100)/meandl9
}

# Variability of annual minimum of 90-day moving average flow. Compute the 
# standard deviation for the minimum 90-day moving averages. DL10 is 100 times 
# the standard deviation divided by the mean (percent - spatial).
dl10<-function(datafile){
    #Compute mean of dl5
    meandl10<-dl5(datafile,pref='mean')
    #Compute standard deviation of dl5 values by year
    sddl10<-sd(min90daybyyear$x)
    dl10<-(sddl10*100)/meandl10
}

# Number of zero-flow days. Count the number of zero-flow days for the entire 
# flow record. DL18 is the mean (or median - Use Preference option) annual 
# number of zero flow days (number of days/year – temporal).
dl18<-function(datafile,pref='mean'){
    #For each year, subset data and count number of days flow is less than 0.01
    #Get number of years in data
    noyears<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    hfcountzeros<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
        #print(i)
        subsetyr<-subset(datafile,datafile$year_val==i)
        echfcrit<-subset(subsetyr,subsetyr$p99_00060_00003<0.001)
        counter<-counter+1
        hfcountzeros[counter]<-length(echfcrit$p99_00060_00003)
    }
    hfcntzeros<<-hfcountzeros
    if(pref=='median') {dl18<-median(hfcntzeros)} else {dl18<-mean(hfcntzeros)}    
}

# Annual maximum of 90-day moving average flows. Compute the maximum of a 90day 
# moving average flow for each year. DH5 is the mean (or median - Use Preference 
# option) of these values (cubic feet per second – temporal).
dh5<-function(datafile,pref='mean'){
    #Compute the 90-day rolling mean
    day90mean<-rollmean(datafile$p99_00060_00003,90,align="right",na.pad=TRUE)
    #Add the day90mean series to the dataframe
    day90rollingavg<-data.frame(datafile,day90mean)
    #Subset rollongavgs90 so that there are no NA values
    rollingavgs90day<-subset(day90rollingavg,day90rollingavg$day90mean!="NA")
    #Compute annual maximum flow for the 90-day rolling mean for each year
    max90daybyyear<<-aggregate(rollingavgs90day$day90mean,list(rollingavgs90day$year_val),max)
    if(pref=='median') {dh5<-median(max90daybyyear$x)} else {dh5<-mean(max90daybyyear$x)}
}

# Variability of annual maximum of 90-day moving average flows. Compute the 
# standard deviation for the maximum 90-day moving averages. DH10 is 100 times 
# the standard deviation divided by the mean (percent – spatial).
dh10<-function(datafile){
    #Compute mean of dl5
    meandh10<-dh5(datafile,pref='mean')
    #Compute standard deviation of dh5 values by year
    sddh10<-sd(max90daybyyear$x)
    dh10<-(sddh10*100)/meandh10
}

# Julian date of annual minimum. Determine the Julian date that the minimum flow
# occurs for each water year. Transform the dates to relative values on a circular scale
# (radians or degrees). Compute the x and y components for each year and average
# them across all years. Compute the mean angle as the arc tangent of y-mean divided
# by x-mean. Transform the resultant angle back to Julian date (Julian day – spatial).
tl1<-function(datafile,pref='mean'){
    #Find the min flow each year
    min1daybyyear<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), min)
    #Get number of years in data
    noyears<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), FUN=median)
    colnames(noyears)<-c("Year","momin")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    juldaymin<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
        #print(i)
        counter<-counter+1
        subsetyr<-subset(datafile,datafile$year_val==i)
        findjulday<-subset(subsetyr,subsetyr$p99_00060_00003==min1daybyyear$x[counter])
        juldaymin[counter]<-findjulday$jul_val[1]
    }
   minjulday<<-juldaymin
   if(pref=='median') {tl1<-median(minjulday)} else {tl1<-mean(minjulday)}
}

# Variability in Julian date of annual minima. Compute the coefficient of variation for
# the mean x and y components and convert to a date (Julian day – spatial).
tl2<-function(datafile){
    #Compute mean of tl1
    meantl2<-tl1(datafile,pref='mean')
    #Compute standard deviation of tl1 values by year
    sddtl2<-sd(minjulday)
    tl2<-(sddtl2*100)/meantl2
}

# Julian date of annual maximum. Determine the Julian date that the maximum flow
# occurs for each year. Transform the dates to relative values on a circular scale
# (radians or degrees). Compute the x and y components for each year and average
# them across all years. Compute the mean angle as the arc tangent of y-mean 
# divided by x-mean. Transform the resultant angle back to Julian date 
# (Julian day – spatial).
th1<-function(datafile,pref='mean'){
    #Find the max flow each year
    max1daybyyear<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), max)
    #Get number of years in data
    noyears<-aggregate(datafile$p99_00060_00003, list(datafile$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    juldaymax<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
        #print(i)
        counter<-counter+1
        subsetyr<-subset(datafile,datafile$year_val==i)
        findjulday<-subset(subsetyr,subsetyr$p99_00060_00003==max1daybyyear$x[counter])
        juldaymax[counter]<-findjulday$jul_val[1]
    }
   maxjulday<<-juldaymax
   if(pref=='median') {th1<-median(maxjulday)} else {th1<-mean(maxjulday)}
}

# Variability in Julian date of annual maxima. Compute the coefficient of 
# variation for the mean x and y components and convert to a date 
# (Julian days - spatial).
th2<-function(datafile){
    #Compute mean of th1
    meanth2<-th1(datafile,pref='mean')
    #Compute standard deviation of th1 values by year
    sddth2<-sd(maxjulday)
    th2<-(sddth2*100)/meanth2
}

# Rise rate. Compute the change in flow for days in which the change is 
# positive for the entire flow record. RA1 is the mean (or median - Use 
# Preference option) of these values (cubic feet per second/day – temporal).
ra1<-function(datafile,pref='mean'){
    #Compute difference between xt and xt+2    
    diffbtdays<-diff(datafile$p99_00060_00003,lag=1,differences=1)
    #Query differences for only positive values
    findrisevalues<<-subset(diffbtdays,diffbtdays>0)
    #Compute median or mean of rise values
    if(pref=='median') {ra1<-median(findrisevalues)} else {ra1<-mean(findrisevalues)}
}

# Fall rate. Compute the change in flow for days in which the change is negative 
# for the entire flow record. RA3 is the mean (or median – Use Preference option) 
# of these values (cubic feet per second/day – temporal).
ra3<-function(datafile,pref='mean'){
    #Compute difference between xt and xt+2    
    diffbtdays<-diff(datafile$p99_00060_00003,lag=1,differences=1)
    #Query differences for only positive values
    findfallvalueneg<-subset(diffbtdays,diffbtdays<0)
    findfallvalues<<-abs(findfallvalueneg)
    #Compute median or mean of rise values
    if(pref=='median') {ra3<-median(findfallvalues)} else {ra3<-mean(findfallvalues)}
}

# Variability in fall rate. Compute the standard deviation for the negative flow 
# changes. RA4 is 100 times the standard deviation divided by the mean 
# (percent – spatial).
ra4<-function(datafile){
    #Compute mean of ra3
    meanra4<-ra3(datafile,pref='mean')
    #Compute standard deviation of tl1 values by year
    sddra4<-sd(findfallvalues)
    ra4<-(sddra4*100)/meanra4
}

#------Miscellaneous but frequently used functions-------

# Compute median flow by year
medflowbyyear<-function(datafile){aggregate(datafile$p99_00060_00003,list(datafile$year_val),median)}

# Compute mean flow by year
medflowbyyear<-function(datafile){aggregate(datafile$p99_00060_00003,list(datafile$year_val),mean)}

# Compute the base-flow index by year
bfi<-function(datafile){   
    #Compute minimum 7-day rolling average for all flows in the streamflow record
    day7mean<-rollmean(datafile$p99_00060_00003,7,align="right",na.pad=TRUE)
    #Make new dataframe to store day7mean with years
    rollingavg<-data.frame(datafile,day7mean)
    #Subset rollongavgs1 so that there are no NA values
    rollingavgs7day<-subset(rollingavg,rollingavg$day7mean!="NA")
    #Compute annual minimum flow for the 7-day rolling mean for each year
    min7daybyyear<-aggregate(rollingavgs7day$day7mean,list(rollingavgs7day$year_val),min)
    #Compute mean flow for each year
    meanflow<-medflowbyyear(datafile)
    #Merge minimum 7-day rolling average with mean flow for each year
    compbfi<-merge(min7daybyyear,meanflow,by="Group.1")
    colnames(compbfi)<-c("year","day7min","meandaily")
    #Compute ML17 by year
    bfi<-compbfi$day7min/compbfi$meandaily
}

# Find the rank value of the data point that is equal to the specified
# exceedence probability
findrank<-function(n,p){
    r<-(1-p)*(n+1)
    findrank<-floor(r)
}

#Function to compute all statistics for a single time series
compallstatsJEK<-function(datafile,typeofstat){

  # compute all stats
  # the matrix gets converted to a dataframe in the wrapper program - why not
  # just make it into a dataframe now?
  # but does that create problems with indexing of output for more than one station?
  # can probably work something out with data.matrix, though mixed data types might pose a problem
  flowstats$ma1 = ma1(datafile)
  flowstats$ma2 = ma2(datafile)
  flowstats$ma5 = ma5(datafile)

  strmo<-ma16.23(datafile,pref=typeofstat) #then picks out specific stats
  # as written, one value per array - no mixture of lengths.
  statsres[2]<-strmo$x[5]  #ma16 - May  
  statsres[3]<-strmo$x[7]  #ma18 - July
  statsres[4]<-strmo$x[10]  #ma21 - October
  statsres[5]<-strmo$x[11]  #ma22 - November
  strmocv<-ma24.35(datafile,pref=typeofstat)
  statsres[6]<-strmocv$x[3]  #ma26 - March
  statsres[7]<-strmocv$x[6]  #ma29 - June
  statsres[8]<-strmocv$x[11]  #ma34 - November
  statsres[9]<-ma37(datafile)
  statsres[10]<-ma39(datafile)
  statsres[11]<-ml13(datafile)
  statsres[12]<-ml14(datafile)
  statsres[13]<-ml17(datafile,pref=typeofstat)
  statsres[14]<-ml18(datafile)
  statsres[15]<-mh14(datafile)
  statsres[16]<-mh16(datafile)
  statsres[17]<-mh26(datafile)
  statsres[18]<-fh2(datafile)
  statsres[19]<-fh3(datafile,pref=typeofstat)
  statsres[20]<-fh4(datafile,pref=typeofstat)
  statsres[21]<-dh5(datafile,pref=typeofstat)
  statsres[22]<-dh10(datafile)
  statsres[23]<-fl1(datafile,pref=typeofstat)
  statsres[24]<-fl2(datafile)
  statsres[25]<-th1(datafile,pref=typeofstat)
  statsres[26]<-th2(datafile)
  statsres[27]<-tl1(datafile,pref=typeofstat)
  statsres[28]<-tl2(datafile)
  statsres[29]<-ra1(datafile,pref=typeofstat)
  statsres[30]<-ra4(datafile)
  statsres[31]<-dl1(datafile,pref=typeofstat)
  statsres[32]<-dl2(datafile,pref=typeofstat)
  statsres[33]<-dl5(datafile,pref=typeofstat)
  statsres[34]<-dl6(datafile)
  statsres[35]<-dl9(datafile)
  statsres[36]<-dl10(datafile)
  statsres[37]<-dl18(datafile,pref=typeofstat)
  compallstats<-flowstats
}

#Function that ran all of Stacey's stats (limited for monthly)
compallstats<-function(datafile,typeofstat){
  statsres<-NULL
  #Number if stats
  k<-37
  statsres<-rep(0,k)
  #All stats here
  statsres[1]<-ma5(datafile)
  strmo<-ma16.23(datafile,pref=typeofstat) #then picks out specific stats
  statsres[2]<-strmo$x[5]  #ma16 - May  
  statsres[3]<-strmo$x[7]  #ma18 - July
  statsres[4]<-strmo$x[10]  #ma21 - October
  statsres[5]<-strmo$x[11]  #ma22 - November
  strmocv<-ma24.35(datafile,pref=typeofstat)
  statsres[6]<-strmocv$x[3]  #ma26 - March
  statsres[7]<-strmocv$x[6]  #ma29 - June
  statsres[8]<-strmocv$x[11]  #ma34 - November
  statsres[9]<-ma37(datafile)
  statsres[10]<-ma39(datafile)
  statsres[11]<-ml13(datafile)
  statsres[12]<-ml14(datafile)
  statsres[13]<-ml17(datafile,pref=typeofstat)
  statsres[14]<-ml18(datafile)
  statsres[15]<-mh14(datafile)
  statsres[16]<-mh16(datafile)
  statsres[17]<-mh26(datafile)
  statsres[18]<-fh2(datafile)
  statsres[19]<-fh3(datafile,pref=typeofstat)
  statsres[20]<-fh4(datafile,pref=typeofstat)
  statsres[21]<-dh5(datafile,pref=typeofstat)
  statsres[22]<-dh10(datafile)
  statsres[23]<-fl1(datafile,pref=typeofstat)
  statsres[24]<-fl2(datafile)
  statsres[25]<-th1(datafile,pref=typeofstat)
  statsres[26]<-th2(datafile)
  statsres[27]<-tl1(datafile,pref=typeofstat)
  statsres[28]<-tl2(datafile)
  statsres[29]<-ra1(datafile,pref=typeofstat)
  statsres[30]<-ra4(datafile)
  statsres[31]<-dl1(datafile,pref=typeofstat)
  statsres[32]<-dl2(datafile,pref=typeofstat)
  statsres[33]<-dl5(datafile,pref=typeofstat)
  statsres[34]<-dl6(datafile)
  statsres[35]<-dl9(datafile)
  statsres[36]<-dl10(datafile)
  statsres[37]<-dl18(datafile,pref=typeofstat)
  compallstatsSarch<-statsres
}

# sortq<-datafile[with(datafile, order(datafile$p99_00060_00003)), ]


