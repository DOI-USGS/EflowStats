# wps.des: id=output, title = output stats, abstract = Finds the mean daily flow median daily flow and skewness of daily flow in the input dataset;
# wps.in: input, string, SOS Endpoint, A fully formed SOS GetObservations request that will return a SWE common CSV block holding date and flow;

# input="http://cida.usgs.gov/qa/climate/derivative/proxy/http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/SYE.nc?service=SOS&request=GetObservation&version=1.0.0&offering=02177000&observedProperty=obsq"

library("XML")
library(zoo)
library(chron)
library(doBy)
library(dataRetrieval)

SWE_CSV_IHA <-
    function(input)
{
  cat(paste("Retrieving data from: \n", input, "\n",sep=" "))
  flow <- read.delim(header = F, comment.char = "", as.is = T, sep=",",text=xpathApply(xmlParse(input),"//swe:values", xmlValue)[[1]])
  nms <- c("date","discharge")
  names(flow) <- nms
  flow$date<- as.POSIXct(strptime(flow$date, format="%Y-%m-%dT%H:%M:%SZ"))
  flow$discharge <- as.numeric(flow$discharge)
  flow <- as.data.frame(flow)
  attr(flow, 'SRC') <- input
  class(flow) <- c('flow', 'data.frame')
  cat("Finished!\n")
  return(flow)
}
#setwd('/Users/jlthomps/Documents/R/')
#x<-SWE_CSV_IHA('http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/afinch/afinch-Scenario-0.1.nc?request=GetObservation&service=SOS&version=1.0&offering=02177000&observedProperty=MEAN')
#x<-SWE_CSV_IHA(input)
#a<-read.table("sites_test.txt",header=F,colClasses=c("character"))
a<-read.csv("sites_test.txt",header=F,colClasses=c("character"))
#colnames(a)<-c("site_no")
a2<-length(a)
yv<-vector(length=a2)
ma1v<-vector(length=a2)
ma2v<-vector(length=a2)
ma3v<-vector(length=a2)
ma5v<-vector(length=a2)
ma12v<-vector(length=a2)
ma13v<-vector(length=a2)
ma14v<-vector(length=a2)
ma15v<-vector(length=a2)
ma16v<-vector(length=a2)
ma17v<-vector(length=a2)
ma18v<-vector(length=a2)
ma19v<-vector(length=a2)
ma20v<-vector(length=a2)
ma21v<-vector(length=a2)
ma22v<-vector(length=a2)
ma23v<-vector(length=a2)
ma24v<-vector(length=a2)
ma25v<-vector(length=a2)
ma26v<-vector(length=a2)
ma27v<-vector(length=a2)
ma28v<-vector(length=a2)
ma29v<-vector(length=a2)
ma30v<-vector(length=a2)
ma31v<-vector(length=a2)
ma32v<-vector(length=a2)
ma33v<-vector(length=a2)
ma34v<-vector(length=a2)
ma35v<-vector(length=a2)
ma37v<-vector(length=a2)
ma39v<-vector(length=a2)
ma40v<-vector(length=a2)
ml13v<-vector(length=a2)
ml14v<-vector(length=a2)
mh14v<-vector(length=a2)
mh16v<-vector(length=a2)
mh26v<-vector(length=a2)
ml17v<-vector(length=a2)
ml18v<-vector(length=a2)
fl1v<-vector(length=a2)
fl2v<-vector(length=a2)
fh1v<-vector(length=a2)
fh2v<-vector(length=a2)
fh3v<-vector(length=a2)
fh4v<-vector(length=a2)
dl1v<-vector(length=a2)
dl2v<-vector(length=a2)
dl4v<-vector(length=a2)
dl5v<-vector(length=a2)
dl6v<-vector(length=a2)
dl9v<-vector(length=a2)
dl10v<-vector(length=a2)
dl18v<-vector(length=a2)
dh5v<-vector(length=a2)
dh10v<-vector(length=a2)
tl1v<-vector(length=a2)
tl2v<-vector(length=a2)
th1v<-vector(length=a2)
th2v<-vector(length=a2)
ra1v<-vector(length=a2)
ra3v<-vector(length=a2)
ra4v<-vector(length=a2)


for (i in 1:length(a)){
  #print(i)
  site<-a[i]
  url<-paste('http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/thredds/sos/watersmart/afinch/afinch-Scenario-0.1.nc?request=GetObservation&service=SOS&version=1.0&offering=',site,'&observedProperty=MEAN',sep='',collapse=NULL)
  x<-SWE_CSV_IHA(url)
#x<-read.delim("dv_test.txt",header=T)
colnames(x)<-c("date","discharge")
x2<-(x$date)
#x<-data.frame(strptime(x2,"%m/%d/%Y"),x$discharge)
x<-data.frame(strptime(x2, "%Y-%m-%d"),x$discharge)
colnames(x)<-c("date","discharge")
yv[i]<-as.character(min(x$date))

#y=as.character(y[1])
# This function creates additional variables to the original file to be used
# in the calculation of the flow statistics. This function assumes that the
# input file has column named 'date' with the date formatted as YYYY-MM-DD
# and flow is located in a column named 'discharge'. Other format 
# functions can be developed to accodate other input formats. 
#flow.data<-function(x,incremnt,syr,eyr){ 
  #Query time series for subset of length equal to the increment
  selqfile<-x
  tempdatafr<-NULL
  tempdatafr<-data.frame(selqfile)
  #Create additional variables for each day
  month_val<-rep(0,length(tempdatafr$date))
  year_val<-rep(0,length(tempdatafr$date))
  day_val<-rep(0,length(tempdatafr$date))
  jul_val<-rep(0,length(tempdatafr$date))
  #Add additonal variables to the dataframe
  qfiletempf<-data.frame(tempdatafr$date,tempdatafr$discharge,month_val,year_val,day_val,jul_val)
  colnames(qfiletempf)<-c('date','discharge','month_val','year_val','day_val','jul_val')
  #Compute the additional variables to the dataframe
  qfiletempf$month_val<-substr(x$date,6,7)
  as.numeric(qfiletempf$month_val)
  qfiletempf$year_val<-substr(x$date,3,4)
  as.numeric(qfiletempf$year_val)
  qfiletempf$day_val<-substr(x$date,9,10)
  as.numeric(qfiletempf$day_val)
  qfiletempf$jul_val<-strptime(x$date, "%Y-%m-%d")$yday+1
  as.numeric(qfiletempf$jul_val)
  flowdata<-data.frame(qfiletempf$date,qfiletempf$discharge,qfiletempf$month_val,qfiletempf$year_val,qfiletempf$day_val,qfiletempf$jul_val)
  colnames(flowdata)<-c('date','discharge','month_val','year_val','day_val','jul_val')
#} 

# Compute median flow by year
medflowbyyear<-function(qfiletempf){aggregate(qfiletempf$discharge,list(qfiletempf$year_val),median)}

# Compute mean flow by year
meanflowbyyear<-function(qfiletempf){aggregate(qfiletempf$discharge,list(qfiletempf$year_val),mean)}

# Compute the base-flow index by year
bfi<-function(qfiletempf){   
  #Compute minimum 7-day rolling average for all flows in the streamflow record
  day7mean<-rollmean(qfiletempf$discharge,7,align="right",na.pad=TRUE)
  #Make new dataframe to store day7mean with years
  rollingavg<-data.frame(qfiletempf,day7mean)
  #Subset rollongavgs1 so that there are no NA values
  rollingavgs7day<-subset(rollingavg,rollingavg$day7mean!="NA")
  #Compute annual minimum flow for the 7-day rolling mean for each year
  min7daybyyear<-aggregate(rollingavgs7day$day7mean,list(rollingavgs7day$year_val),min)
  #Compute mean flow for each year
  meanflow<-meanflowbyyear(qfiletempf)
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

# Mean of the daily mean flow values for the entire flow record 
# (cubic feet per second – temporal).
ma1<-function(x){ma1<-mean(x$discharge)}
ma1v[i]<-ma1(x)

# Median of the daily mean flow values for the entire flow record 
# (cubic feet per second – temporal).
ma2<-function(x){ma2<-median(x$discharge)}
ma2v[i]<-ma2(x)

# Mean (or median) of the coefficents of variation (st dev/mean) for each year. 
# Compute the cv for each year of daily flows. Compute the mean of the annual 
# coefficients of variation (percent-temporal)
ma3<-function(qfiletempf,pref='mean'){
    sdbyyr<-aggregate(qfiletempf$discharge,list(qfiletempf$year_val),FUN=sd)
    colnames(sdbyyr)<-c("Year","sdq")
    meanbyyr<-aggregate(qfiletempf$discharge,list(qfiletempf$year_val),mean)
    colnames(meanbyyr)<-c("Year","meanq")
    dfcvbyyr<-data.frame(meanbyyr$Year, sdbyyr$sdq, meanbyyr$meanq)
    colnames(dfcvbyyr)<-c("Year","sdq","meanq")
    cvbyyr<-dfcvbyyr$sdq/dfcvbyyr$meanq
    dfcvbyyrf<-data.frame(dfcvbyyr,cvbyyr)
    colnames(dfcvbyyrf)<-c("Year","sdq","meanq","cvq")
    #Compute the coefficient of variation of the annual flows by year
    if(pref=='median') {
      medcv<-median(dfcvbyyrf$cvq)
      ma3<-(medcv)*100}
    else {
      meancv<-mean(dfcvbyyrf$cvq)
      ma3<-(meancv)*100}
}
ma3v[i]<-ma3(qfiletempf)

# The skewness of the entire flow record is computed as the mean for 
# the entire flow record (MA1) divided by the median (MA2) for the 
# entire flow record (dimensionless - spatial).
ma5<-function(x){x1<-ma1(x); x2<-ma2(x); ma5<-x1/x2}
ma5v[i]<-ma5(x)

  # Means (or medians - Use Preference option) of monthly flow values.
  # For example, MA12 is the mean of all January flow values over the 
  # entire record (cubic feet per second – temporal)
  ma12.23<-function(qfiletempf,pref='mean'){
    if(pref=='median') {
      medmon<-aggregate(qfiletempf$discharge, list(qfiletempf$month_val), median)
      ma12.23<-data.frame(medmon)}
    else {
      meanmon<-aggregate(qfiletempf$discharge, list(qfiletempf$month_val), mean)
      ma12.23<-data.frame(meanmon)}
  }

  ma12v[i]<-ma12.23(qfiletempf)[1:1,2:2]
  ma13v[i]<-ma12.23(qfiletempf)[2:2,2:2]
  ma14v[i]<-ma12.23(qfiletempf)[3:3,2:2]
  ma15v[i]<-ma12.23(qfiletempf)[4:4,2:2]
  ma16v[i]<-ma12.23(qfiletempf)[5:5,2:2]
  ma17v[i]<-ma12.23(qfiletempf)[6:6,2:2]
  ma18v[i]<-ma12.23(qfiletempf)[7:7,2:2]
  ma19v[i]<-ma12.23(qfiletempf)[8:8,2:2]
  ma20v[i]<-ma12.23(qfiletempf)[9:9,2:2]
  ma21v[i]<-ma12.23(qfiletempf)[10:10,2:2]
  ma22v[i]<-ma12.23(qfiletempf)[11:11,2:2]
  ma23v[i]<-ma12.23(qfiletempf)[12:12,2:2]

  # Variability (coefficient of variation) of monthly flow values. Compute the
  # MA35 standard deviation for each month in each year over the entire flow 
  # record. Divide the standard deviation by the mean for each month. Average 
  # (or median - Use Preference option) these values for each month across 
  # all years (percent – temporal)
  ma24.35<-function(qfiletempf,pref='mean'){
    #Compute standard deviation of the monthly flows by year
    sdmonbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN=sd)
    colnames(sdmonbyyr)<-c("Year","Month","sdq")
    #Compute mean of the monthly flows by year
    meanmonbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN=mean)
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
      ma24.35<-data.frame(medmoncv[2]*100)}
    else {
      meanmoncv<-aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), mean)
      ma24.35<-data.frame(meanmoncv[2]*100)}
  }

  ma24v[i]<-ma24.35(qfiletempf)[1,1]
  ma25v[i]<-ma24.35(qfiletempf)[2,1]
  ma26v[i]<-ma24.35(qfiletempf)[3,1]
  ma27v[i]<-ma24.35(qfiletempf)[4,1]
  ma28v[i]<-ma24.35(qfiletempf)[5,1]
  ma29v[i]<-ma24.35(qfiletempf)[6,1]
  ma30v[i]<-ma24.35(qfiletempf)[7,1]
  ma31v[i]<-ma24.35(qfiletempf)[8,1]
  ma32v[i]<-ma24.35(qfiletempf)[9,1]
  ma33v[i]<-ma24.35(qfiletempf)[10,1]
  ma34v[i]<-ma24.35(qfiletempf)[11,1]
  ma35v[i]<-ma24.35(qfiletempf)[12,1]

  # Variability across monthly flows. Compute the first (25th percentile) and the 
  # third (75th percentile) quartiles (every month in the flow record). MA37 is 
  # the third quartile minus the first quartile divided by the median of the 
  # monthly means (dimensionless – spatial).
  ma37<-function(qfiletempf){
    #Compute mean flow by month and year
    sdmonbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN=mean)
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
ma37v[i]<-unname(ma37(qfiletempf))
  
  # Variability across monthly flows. Compute the standard deviation for the 
  # monthly means. MA39 is the standard deviation times 100 divided by the mean 
  # of the monthly means (percent – spatial).
  ma39<-function(qfiletempf){
    #Compute mean flow by month and year
    sdmonbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN=mean)
    colnames(sdmonbyyr)<-c("Year","Month","meanmo")    
    #Compute standard deviation of monthly flows
    sdmonflows<-sd(sdmonbyyr$meanmo)
    #Compute the mean of the monthly flows
    meanmonflows<-mean(sdmonbyyr$meanmo)
    ma39<-(sdmonflows*100)/meanmonflows
  }
ma39v[i]<-ma39(qfiletempf)
  
  # Skewness in the monthly flows. MA40 is the mean of the monthly flow means 
  # minus the median of the monthly means divided by the median of the monthly 
  # means (dimensionless – spatial).
  ma40<-function(qfiletempf){
    numer1<-colMeans(ma12.23(qfiletempf)[2])
    numer2<-apply(ma12.23(qfiletempf,pref='median')[2],2,median)
    ma40<-(numer1-numer2)/numer2      
  }
ma40v[i]<-unname(ma40(qfiletempf))

  # Variability (coefficient of variation) across minimum monthly flow values. 
  # Compute the mean and standard deviation for the minimum monthly flows over 
  # the entire flow record. ML13 is the standard deviation times 100 divided by 
  # the mean minimum monthly flow for all years (percent – spatial).
  ml13<-function(qfiletempf){
    #Compute mean flow by month and year
    minmonbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN=min)
    colnames(minmonbyyr)<-c("Year","Month","minmo")
    #Compute standard deviation of min monthly flows
    sdminmonflows<-sd(minmonbyyr$minmo)
    #Compute the mean of the min monthly flows
    meanminmonflows<-mean(minmonbyyr$minmo)
    ml13<-(sdminmonflows*100)/meanminmonflows
  }
  ml13v[i]<-ml13(qfiletempf)
  # Compute the minimum annual flow for each year. ML14 is the mean of the 
  # ratios of minimum annual flows to the median flow for each year 
  # (dimensionless – temporal).
  ml14<-function(qfiletempf){
    #Compute 1-day mean minimum flow for each year
    min1daybyyear<-aggregate(qfiletempf$discharge,list(qfiletempf$year_val),min)
    #Compute median annual flow for each year
    medflow<-aggregate(qfiletempf$discharge,list(qfiletempf$year_val),median)
    #Divide 1-day mean minimum flow by the median annual flow for each year
    computeml14<-merge(min1daybyyear,medflow,by="Group.1")
    colnames(computeml14)<-c("year","day1min","meddaily")
    dfml14<-computeml14$day1min/computeml14$meddaily
    #Compute mean of annual minimum
    ml14<-mean(dfml14)
  }
  ml14v[i]<-ml14(qfiletempf)
  
  # Base flow. Compute the mean annual flows. Compute the minimum of a 
  # 7-day moving average flow for each year and divide them by the mean 
  # annual flow for that year. ML17 is the mean (or median - Use Preference 
  # option) of those ratios (dimensionless – temporal).
  ml17<-function(qfiletempf,pref='mean'){
    #Use the function bfi to compute the base-flow index for each year
    bfibyyear<-bfi(qfiletempf)
    #Compute the coefficient of variation of the monthly flows by year
    if(pref=='median') {ml17<-median(bfibyyear)} else {ml17<-mean(bfibyyear)}
  }
  ml17v[i]<-ml14(qfiletempf)
  
  # Variability in base flow. Compute the standard deviation for the ratios of 
  # 7-day moving average flows to mean annual flows for each year. ML18 is the 
  # standard deviation times 100 divided by the mean of the ratios 
  # (percent – spatial).
  ml18<-function(qfiletempf){
    #Use the function bfi to compute the base-flow index for each year
    bfibyyear<-bfi(qfiletempf)
    #Compute the standard deviation of the base-flow index
    sdbfi<-sd(bfibyyear)
    meanbfi<-mean(bfibyyear)
    #Compute the coefficient of variation of the base-flow index
    ml18<-meanbfi/sdbfi
  }
  ml18v[i]<-ml18(qfiletempf)
  
  # Median of annual maximum flows. Compute the annual maximum flows from monthly 
  # maximum flows. Compute the ratio of annual maximum flow to median annual flow 
  # for each year. MH14 is the median of these ratios (dimensionless – temporal).
  mh14<-function(qfiletempf){
    #Compute maximum flow by year and month
    maxmonbymoyr<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN=max)
    colnames(maxmonbymoyr)<-c("Year","Month","momax")
    #Compute the maximum of the monthly flows by year
    maxmonbyyrr<-aggregate(maxmonbymoyr$momax, list(maxmonbymoyr$Year), FUN=max)
    colnames(maxmonbyyrr)<-c("Year","yrmax")
    #Compute the median flow for each year
    medflowbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), FUN=median)
    colnames(medflowbyyr)<-c("Year","yrmed")
    #Divide the maximum of the monthly flows by year by the median flow for each year
    ratiomaxmed<-maxmonbyyrr$yrmax/medflowbyyr$yrmed
    #Compute the median of ratiomaxmed
    mh14<-median(ratiomaxmed)   
  }
  mh14v[i]<-mh14(qfiletempf)
  
  # High flow discharge index. Compute the 10 percent exceedence value for the 
  # entire data record. MH16 is the 10 percent exceedence value divided by the 
  # median flow for the entire record (dimensionless – spatial).
  mh16<-function(qfiletempf){
    #Isolate the daily streamflow values    
    isolateq<-qfiletempf$discharge
    #Sort the daily streamflow values
    sortq<-sort(isolateq)
    #Find rank value in sortq that equals the 10-percent exceedence value
    frank<-floor(findrank(length(sortq),0.10))
    #Find streamflow value corresponding to the rank
    hfcrit<-sortq[frank]
    #Divide hfcrit by MA2 (median flow)
    mh16<-hfcrit/ma2(x)
  }
  mh16v[i]<-mh16(qfiletempf)
  
  # High peak flow. Compute the average peak flow value for flow events above 
  # a threshold equal to seven times the median flow for the entire record. 
  # MH26 is the average peak flow divided by the median flow for the entire 
  # record (dimensionless – temporal).
  mh26<-function(qfiletempf){
    #Compute threshold (7*MA2)
    hfcrit<-7*ma2(qfiletempf)
    #Isolate the daily streamflow values    
    isolateq<-qfiletempf$discharge
    #Query flow record for all events greater than hfcrit
    exchfcrit<-subset(isolateq,isolateq>hfcrit)
    #Take mean of streamflow values that exceed hfcrit
    meanex<-mean(exchfcrit)
    #Divide meanex by MA2
    mh26<-meanex/ma2(x)
  }
  mh26v[i]<-mh26(qfiletempf)

  # Low flood pulse count. Compute the average number of flow events with flows 
  # below a threshold equal to the 25th percentile value for the entire flow record. 
  # FL1 is the average (or median - Use Preference option) number of events 
  # (number of events/year – temporal).
  fl1<-function(qfiletempf,pref='mean'){
    #Isolate the daily streamflow values    
    isolateq<-qfiletempf$discharge
    #Sort the daily streamflow values
    sortq<-sort(isolateq)
    #Find rank value in sortq that equals the 10-percent exceedence value
    frank<-floor(findrank(length(sortq),0.75))
    #Find streamflow value corresponding to the rank
    lfcrit<-sortq[frank]
    #For each year, subset data and count number of days flow exceeds threshold
    #Get number of years in data
    noyears<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    lfcountbyyr<-rep(0,noyrs)
    counter<-0
    for (i in as.character(noyears$Year[1]):as.character(noyears$Year[noyrs])){
      #print(i)
      subsetyr<-subset(qfiletempf,as.numeric(qfiletempf$year_val)==i)
      echfcrit<-subset(subsetyr,subsetyr$discharge<lfcrit)
      counter<-counter+1
      lfcountbyyr[counter]<-length(echfcrit$discharge)
    }
    lfcntbyyr<<-lfcountbyyr
    if(pref=='median') {fl1<-median(lfcntbyyr)} else {fl1<-mean(lfcntbyyr)}
  }
  fl1v[i]<-fl1(qfiletempf)
  
  # Variability in low pulse count. Compute the standard deviation in the annual 
  # pulse counts for FL1. FL2 is 100 times the standard deviation divided by the 
  # mean pulse count (percent – spatial).
  fl2<-function(qfiletempf){
    #Call function for fl1 to generate hfcountbyyr
    meanfl1<-fl1(qfiletempf,pref='mean')    
    #Compute stdev of hfcountbyyr
    stdevfl1<-sd(lfcntbyyr)
    fl2<-(stdevfl1*100)/meanfl1
  }
  fl2v[i]<-fl2(qfiletempf)
  
  # High flood pulse count. Compute the average number of flow events with flows 
  # above a threshold equal to the 75th percentile value for the entire flow record. 
  # FH1 is the average (or median - Use Preference option) number of events 
  # (number of events/year – temporal).
  fh1<-function(qfiletempf,pref='mean'){
    #Isolate the daily streamflow values    
    isolateq<-qfiletempf$discharge
    #Sort the daily streamflow values
    sortq<-sort(isolateq)
    #Find rank value in sortq that equals the 10-percent exceedence value
    frank<-floor(findrank(length(sortq),0.25))
    #Find streamflow value corresponding to the rank
    hfcrit<-sortq[frank]
    #For each year, subset data and count number of days flow exceeds threshold
    #Get number of years in data
    noyears<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    hfcountbyyr<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
      #print(i)
      subsetyr<-subset(qfiletempf,as.numeric(qfiletempf$year_val)==i)
      echfcrit<-subset(subsetyr,subsetyr$discharge>hfcrit)
      counter<-counter+1
      hfcountbyyr[counter]<-length(echfcrit$discharge)
    }
    hfcntbyyr<<-hfcountbyyr
    if(pref=='median') {fh1<-median(hfcntbyyr)} else {fh1<-mean(hfcntbyyr)}
  }
  fh1v[i]<-fh1(qfiletempf)
  
  # Variability in high pulse count. Compute the standard deviation in the annual 
  # pulse counts for FH1. FH2 is 100 times the standard deviation divided by the 
  # mean pulse count (percent – spatial).
  fh2<-function(qfiletempf){
    #Call function for fl1 to generate hfcountbyyr
    meanfh1<-fh1(qfiletempf,pref='mean')    
    #Compute stdev of hfcountbyyr
    stdevfl1<-sd(hfcntbyyr)
    fh2<-(stdevfl1*100)/meanfh1
  }
  fh2v[i]<-fh2(qfiletempf)
  
  # High flood pulse count. Compute the average number of days per year that the 
  # flow is above a threshold equal to three times the median flow for the entire 
  # record. FH3 is the mean (or median – Use Preference option) of the annual 
  # number of days for all years (number of days/year – temporal).
  fh3<-function(qfiletempf,pref='mean'){
    #Compute threshold (3*MA2)
    hfcrit<-3*ma2(qfiletempf)
    #For each year, subset data and count number of days flow exceeds threshold
    #Get number of years in data
    noyears<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    hfcountbyyrfh3<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
      #print(i)
      subsetyr<-subset(qfiletempf,as.numeric(qfiletempf$year_val)==i)
      echfcrit<-subset(subsetyr,subsetyr$discharge>hfcrit)
      counter<-counter+1
      hfcountbyyrfh3[counter]<-length(echfcrit$discharge)
    }
    hfcntbyyrfh3<<-hfcountbyyrfh3
    if(pref=='median') {fh3<-median(hfcntbyyrfh3)} else {fh3<-mean(hfcntbyyrfh3)}
  }
  fh3v[i]<-fh3(qfiletempf)
  
  # High flood pulse count. Compute the average number of days per year that the 
  # flow is above a threshold equal to seven times the median flow for the entire 
  # record. FH4 is the mean (or median - Use Preference option) of the annual 
  # number of days for all years (number of days/year – temporal).
  fh4<-function(qfiletempf,pref='mean'){
    #Compute threshold (3*MA2)
    hfcrit<-7*ma2(qfiletempf)
    #For each year, subset data and count number of days flow exceeds threshold
    #Get number of years in data
    noyears<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    hfcountbyyrfh4<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
      #print(i)
      subsetyr<-subset(qfiletempf,as.numeric(qfiletempf$year_val)==i)
      echfcrit<-subset(subsetyr,subsetyr$discharge>hfcrit)
      counter<-counter+1
      hfcountbyyrfh4[counter]<-length(echfcrit$discharge)
    }
    hfcntbyyrfh4<<-hfcountbyyrfh4
    if(pref=='median') {fh4<-median(hfcntbyyrfh4)} else {fh4<-mean(hfcntbyyrfh4)}
  }
  fh4v[i]<-fh4(qfiletempf)
  
  # Annual minimum daily flow. Compute the minimum 1-day average flow for each 
  # year. DL1 is the mean (or median - Use Preference option) of these values 
  # (cubic feet per second – temporal).
  dl1<-function(qfiletempf,pref='mean'){
    #Compute the 1-day rolling mean
    day1mean<-rollmean(qfiletempf$discharge,1,align="right",na.pad=TRUE)
    #Add the day1mean series to the dataframe
    day1rollingavg<-data.frame(qfiletempf,day1mean)
    #Subset rollongavgs1 so that there are no NA values
    rollingavgs1day<-subset(day1rollingavg,day1rollingavg$day1mean!="NA")
    #Compute annual minimum flow for the 1-day rolling mean for each year
    min1daybyyear<<-aggregate(rollingavgs1day$day1mean,list(rollingavgs1day$year_val),min)
    if(pref=='median') {dl1<-median(min1daybyyear$x)} else {dl1<-mean(min1daybyyear$x)}
  }
  dl1v[i]<-dl1(qfiletempf)
  
  # Annual minimum of 3-day moving average flow. Compute the minimum of a 3-day 
  # moving average flow for each year. DL2 is the mean (or median - Use Preference 
  # option) of these values (cubic feet per second – temporal).
  dl2<-function(qfiletempf,pref='mean'){
    #Compute the 3-day rolling mean
    day3mean<-rollmean(qfiletempf$discharge,3,align="right",na.pad=TRUE)
    #Add the day3mean series to the dataframe
    day3rollingavg<-data.frame(qfiletempf,day3mean)
    #Subset rollongavgs3 so that there are no NA values
    rollingavgs3day<-subset(day3rollingavg,day3rollingavg$day3mean!="NA")
    #Compute annual minimum flow for the 3-day rolling mean for each year
    min3daybyyear<<-aggregate(rollingavgs3day$day3mean,list(rollingavgs3day$year_val),min)
    if(pref=='median') {dl2<-median(min3daybyyear$x)} else {dl2<-mean(min3daybyyear$x)}
  }
  dl2v[i]<-dl2(qfiletempf)
  
  # Annual minimum of 30-day moving average flow. Compute the minimum of a 30-day 
  # moving average flow for each year. DL4 is the mean (or median - Use Preference 
  # option) of these values (cubic feet per second – temporal).
  dl4<-function(qfiletempf,pref='mean'){
    #Compute the 30-day rolling mean
    day30mean<-rollmean(qfiletempf$discharge,30,align="right",na.pad=TRUE)
    #Add the day30mean series to the dataframe
    day30rollingavg<-data.frame(qfiletempf,day30mean)
    #Subset rollongavgs30 so that there are no NA values
    rollingavgs30day<-subset(day30rollingavg,day30rollingavg$day30mean!="NA")
    #Compute annual minimum flow for the 30-day rolling mean for each year
    min30daybyyear<<-aggregate(rollingavgs30day$day30mean,list(rollingavgs30day$year_val),min)
    if(pref=='median') {dl5<-median(min30daybyyear$x)} else {dl5<-mean(min30daybyyear$x)}
  }
  dl4v[i]<-dl4(qfiletempf)
  
  
  # Annual minimum of 90-day moving average flow. Compute the minimum of a 90-day 
  # moving average flow for each year. DL5 is the mean (or median - Use Preference 
  # option) of these values (cubic feet per second – temporal).
  dl5<-function(qfiletempf,pref='mean'){
    #Compute the 90-day rolling mean
    day90mean<-rollmean(qfiletempf$discharge,90,align="right",na.pad=TRUE)
    #Add the day90mean series to the dataframe
    day90rollingavg<-data.frame(qfiletempf,day90mean)
    #Subset rollongavgs90 so that there are no NA values
    rollingavgs90day<-subset(day90rollingavg,day90rollingavg$day90mean!="NA")
    #Compute annual minimum flow for the 90-day rolling mean for each year
    min90daybyyear<<-aggregate(rollingavgs90day$day90mean,list(rollingavgs90day$year_val),min)
    if(pref=='median') {dl5<-median(min90daybyyear$x)} else {dl5<-mean(min90daybyyear$x)}
  }
  dl5v[i]<-dl5(qfiletempf)
  
  # Variability of annual minimum daily average flow. Compute the standard 
  # deviation for the minimum daily average flow. DL6 is 100 times the standard 
  # deviation divided by the mean (percent – spatial).
  dl6<-function(qfiletempf){
    #Compute mean of dl1
    meandl6<-dl1(qfiletempf,pref='mean')
    #Compute standard deviation of dl1 values by year
    sddl6<-sd(min1daybyyear$x)
    dl6<-(sddl6*100)/meandl6
  }
  dl6v[i]<-dl6(qfiletempf)
  
  # Variability of annual minimum of 30-day moving average flow. Compute the 
  # standard deviation for the minimum 30-day moving averages. DL9 is 100 times 
  # the standard deviation divided by the mean (percent - spatial).
  dl9<-function(qfiletempf){
    #Compute mean of dl4
    meandl9<-dl4(qfiletempf,pref='mean')
    #Compute standard deviation of dl4 values by year
    sddl9<-sd(min30daybyyear$x)
    dl9<-(sddl9*100)/meandl9
  }
  dl9v[i]<-dl9(qfiletempf)
  
  # Variability of annual minimum of 90-day moving average flow. Compute the 
  # standard deviation for the minimum 90-day moving averages. DL10 is 100 times 
  # the standard deviation divided by the mean (percent - spatial).
  dl10<-function(qfiletempf){
    #Compute mean of dl5
    meandl10<-dl5(qfiletempf,pref='mean')
    #Compute standard deviation of dl5 values by year
    sddl10<-sd(min90daybyyear$x)
    dl10<-(sddl10*100)/meandl10
  }
  dl10v[i]<-dl10(qfiletempf)
  
  # Number of zero-flow days. Count the number of zero-flow days for the entire 
  # flow record. DL18 is the mean (or median - Use Preference option) annual 
  # number of zero flow days (number of days/year – temporal).
  dl18<-function(qfiletempf,pref='mean'){
    #For each year, subset data and count number of days flow is less than 0.01
    #Get number of years in data
    noyears<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    hfcountzeros<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
      #print(i)
      subsetyr<-subset(qfiletempf,qfiletempf$year_val==i)
      echfcrit<-subset(subsetyr,subsetyr$discharge<0.001)
      counter<-counter+1
      hfcountzeros[counter]<-length(echfcrit$discharge)
    }
    hfcntzeros<<-hfcountzeros
    if(pref=='median') {dl18<-median(hfcntzeros)} else {dl18<-mean(hfcntzeros)}    
  }
  dl18v[i]<-dl18(qfiletempf)
  
  # Annual maximum of 90-day moving average flows. Compute the maximum of a 90day 
  # moving average flow for each year. DH5 is the mean (or median - Use Preference 
  # option) of these values (cubic feet per second – temporal).
  dh5<-function(qfiletempf,pref='mean'){
    #Compute the 90-day rolling mean
    day90mean<-rollmean(qfiletempf$discharge,90,align="right",na.pad=TRUE)
    #Add the day90mean series to the dataframe
    day90rollingavg<-data.frame(qfiletempf,day90mean)
    #Subset rollongavgs90 so that there are no NA values
    rollingavgs90day<-subset(day90rollingavg,day90rollingavg$day90mean!="NA")
    #Compute annual maximum flow for the 90-day rolling mean for each year
    max90daybyyear<<-aggregate(rollingavgs90day$day90mean,list(rollingavgs90day$year_val),max)
    if(pref=='median') {dh5<-median(max90daybyyear$x)} else {dh5<-mean(max90daybyyear$x)}
  }
  dh5v[i]<-dh5(qfiletempf)
  
  # Variability of annual maximum of 90-day moving average flows. Compute the 
  # standard deviation for the maximum 90-day moving averages. DH10 is 100 times 
  # the standard deviation divided by the mean (percent – spatial).
  dh10<-function(qfiletempf){
    #Compute mean of dl5
    meandh10<-dh5(qfiletempf,pref='mean')
    #Compute standard deviation of dh5 values by year
    sddh10<-sd(max90daybyyear$x)
    dh10<-(sddh10*100)/meandh10
  }
  dh10v[i]<-dh10(qfiletempf)

  # Julian date of annual minimum. Determine the Julian date that the minimum flow
  # occurs for each water year. Transform the dates to relative values on a circular scale
  # (radians or degrees). Compute the x and y components for each year and average
  # them across all years. Compute the mean angle as the arc tangent of y-mean divided
  # by x-mean. Transform the resultant angle back to Julian date (Julian day – spatial).
  tl1<-function(qfiletempf,pref='mean'){
    #Find the min flow each year
    min1daybyyear<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), min)
    #Get number of years in data
    noyears<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), FUN=median)
    colnames(noyears)<-c("Year","momin")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    juldaymin<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
      #print(i)
      counter<-counter+1
      subsetyr<-subset(qfiletempf,as.numeric(qfiletempf$year_val)==i)
      findjulday<-subset(subsetyr,subsetyr$discharge==min1daybyyear$x[counter])
      juldaymin[counter]<-findjulday$jul_val[1]
    }
    minjulday<<-juldaymin
    if(pref=='median') {tl1<-median(minjulday)} else {tl1<-mean(minjulday)}
  }
  tl1v[i]<-tl1(qfiletempf)
  
  # Variability in Julian date of annual minima. Compute the coefficient of variation for
  # the mean x and y components and convert to a date (Julian day – spatial).
  tl2<-function(qfiletempf){
    #Compute mean of tl1
    meantl2<-tl1(qfiletempf,pref='mean')
    #Compute standard deviation of tl1 values by year
    sddtl2<-sd(minjulday)
    tl2<-(sddtl2*100)/meantl2
  }
  tl2v[i]<-tl2(qfiletempf)
  
  # Julian date of annual maximum. Determine the Julian date that the maximum flow
  # occurs for each year. Transform the dates to relative values on a circular scale
  # (radians or degrees). Compute the x and y components for each year and average
  # them across all years. Compute the mean angle as the arc tangent of y-mean 
  # divided by x-mean. Transform the resultant angle back to Julian date 
  # (Julian day – spatial).
  th1<-function(qfiletempf,pref='mean'){
    #Find the max flow each year
    max1daybyyear<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), max)
    #Get number of years in data
    noyears<-aggregate(qfiletempf$discharge, list(qfiletempf$year_val), FUN=median)
    colnames(noyears)<-c("Year","momax")
    noyrs<-length(noyears$Year)
    #Go though each year, query for number of times flow exceeds threshold, 
    #count number of times, place value with year in new dataframe
    juldaymax<-rep(0,noyrs)
    counter<-0
    for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])){
      #print(i)
      counter<-counter+1
      subsetyr<-subset(qfiletempf,as.numeric(qfiletempf$year_val)==i)
      findjulday<-subset(subsetyr,subsetyr$discharge==max1daybyyear$x[counter])
      juldaymax[counter]<-findjulday$jul_val[1]
    }
    maxjulday<<-juldaymax
    if(pref=='median') {th1<-median(maxjulday)} else {th1<-mean(maxjulday)}
  }
  th1v[i]<-th1(qfiletempf)
  
  # Variability in Julian date of annual maxima. Compute the coefficient of 
  # variation for the mean x and y components and convert to a date 
  # (Julian days - spatial).
  th2<-function(qfiletempf){
    #Compute mean of th1
    meanth2<-th1(qfiletempf,pref='mean')
    #Compute standard deviation of th1 values by year
    sddth2<-sd(maxjulday)
    th2<-(sddth2*100)/meanth2
  }
  th2v[i]<-th2(qfiletempf)
  
  # Rise rate. Compute the change in flow for days in which the change is 
  # positive for the entire flow record. RA1 is the mean (or median - Use 
  # Preference option) of these values (cubic feet per second/day – temporal).
  ra1<-function(qfiletempf,pref='mean'){
    #Compute difference between xt and xt+2    
    diffbtdays<-diff(qfiletempf$discharge,lag=1,differences=1)
    #Query differences for only positive values
    findrisevalues<<-subset(diffbtdays,diffbtdays>0)
    #Compute median or mean of rise values
    if(pref=='median') {ra1<-median(findrisevalues)} else {ra1<-mean(findrisevalues)}
  }
  ra1v[i]<-ra1(qfiletempf)
  
  # Fall rate. Compute the change in flow for days in which the change is negative 
  # for the entire flow record. RA3 is the mean (or median – Use Preference option) 
  # of these values (cubic feet per second/day – temporal).
  ra3<-function(qfiletempf,pref='mean'){
    #Compute difference between xt and xt+2    
    diffbtdays<-diff(qfiletempf$discharge,lag=1,differences=1)
    #Query differences for only positive values
    findfallvalueneg<-subset(diffbtdays,diffbtdays<0)
    findfallvalues<<-abs(findfallvalueneg)
    #Compute median or mean of rise values
    if(pref=='median') {ra3<-median(findfallvalues)} else {ra3<-mean(findfallvalues)}
  }
  ra3v[i]<-ra3(qfiletempf)
  
  # Variability in fall rate. Compute the standard deviation for the negative flow 
  # changes. RA4 is 100 times the standard deviation divided by the mean 
  # (percent – spatial).
  ra4<-function(qfiletempf){
    #Compute mean of ra3
    meanra4<-ra3(qfiletempf,pref='mean')
    #Compute standard deviation of tl1 values by year
    sddra4<-sd(findfallvalues)
    ra4<-(sddra4*100)/meanra4
  }
  ra4v[i]<-ra4(qfiletempf)

}

# Make allstats matrix a dataframe with gages added
statsout<-data.frame(t(a),yv,ma1v,ma2v,ma3v,ma5v,ma12v,ma13v,ma14v,ma15v,ma16v,ma17v,ma18v,ma19v,ma20v,ma21v,ma22v,ma23v,
ma24v,ma25v,ma26v,ma27v,ma28v,ma29v,ma30v,ma31v,ma32v,ma33v,ma34v,ma35v,ma37v,ma39v,ma40v,ml13v,ml14v,mh14v,mh16v,
mh26v,ml17v,ml18v,fl1v,fl2v,fh1v,fh2v,fh3v,fh4v,dl1v,dl2v,dl4v,dl5v,dl6v,dl9v,dl10v,dl18v,dh5v,dh10v,tl1v,tl2v,th1v,th2v,ra1v,ra3v,ra4v)

# Assign column names for the statsout dataframe
colnames(statsout)<-c('site_no','min_date','ma1_mean_disc','ma2_median_disc','ma3_mean_annual_var','ma5_skew','ma12_jan_mean','ma13_feb_mean','ma14_mar_mean','ma15_apr_mean',
'ma16_may_mean','ma17_june_mean','ma18_july_mean','ma19_aug_mean','ma20_sep_mean','ma21_oct_mean','ma22_nov_mean','ma23_dec_mean','ma24_jan_var','ma25_feb_var',
'ma26_mar_var','ma27_apr_var','ma28_may_var','ma29_jun_var','ma30_july_var','ma31_aug_var','ma32_sep_var','ma33_oct_var','ma34_nov_var','ma35_dec_var',
'ma37_var_across_months','ma39_monthly_std_dev','ma40_monthly_skewness','ml13_min_monthly_var','ml14_min_annual_flow','mh14_med_annual_max',
'mh16_high_flow_index','mh26_high_peak_flow','ml17_base_flow','ml18_base_flow_var','fl1_low_flood_pulse','fl2_low_pulse_var',
'fh1_high_pulse_count','fh2_high_pulse_var','fh3_high_pulse_count_three','fh4_high_pulse_count_seven','dl1_min_daily_flow',
'dl2_min_3_day_avg','dl4_min_30_day_avg','dl5_min_90_day_avg','dl6_min_flow_var','dl9_min_30_day_var','dl10_min_90_day_var',
'dl18_zero_flow_days','dh5_max_90_day_avg','dh10_max_90_day_var','tl1_min_flow_julian_day','tl2_min_julian_var','th1_max_flow_julian_day',
'th2_max_julian_var','ra1_rise_rate','ra3_fall_rate','ra4_fall_rate_var')
output="allstats.out"
write.table(statsout,file="allstats.out",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

# wps.out: output, text, output_file, A file containing the mean daily flow median daily flow and skewness of daily flow;