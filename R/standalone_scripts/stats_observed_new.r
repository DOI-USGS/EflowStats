
library(XML)
library(zoo)
library(chron)
library(doBy)
library(hydroGOF)
library(HITHATStats)
#library(dataRetrieval)

sos_url="http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering='00003'
property='00060'
drainage_url="http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
site_url="http://cida-wiwsc-gdp2qa.er.usgs.gov:8082/geoserver/nwc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=nwc:se_sites"

getXMLDV2Data <- function(sos_url,sites,property,offering,startdate,enddate,interval,latest){
  
  baseURL <- sos_url
  
  url <- paste(baseURL,sites, "&observedProperty=",property, "&offering=", offering, sep = "")
  
  if (nzchar(startdate)) {
    url <- paste(url,"&beginPosition=",startdate,sep="")
  } else url <- paste(url,"&beginPosition=","1851-01-01",sep="")
  
  if (nzchar(enddate)) {
    url <- paste(url,"&endPosition=",enddate,sep="")
  }
  if (nzchar(interval)) {
    url <- paste(url,"&Interval=",interval,sep="")
  }
  if (nzchar(latest)) {
    url <- paste(url,"&Latest",sep="")
  }
  cat(paste("Retrieving data from: \n", url, "\n", sep = " "))
  
  # Imports the XML:
  doc <- xmlTreeParse(url, getDTD = F, useInternalNodes=TRUE)
  
  values <- xpathSApply(doc, "//om:result//wml2:value")
  values <- sapply(values, function(x) as.numeric(xmlValue(x)))
  dateSet <- getNodeSet(doc, "//om:result//wml2:time")
  dateSet <- lapply(dateSet, function(x) xmlSApply(x, xmlValue))
  dates <- strptime(substr(dateSet,1,10), "%Y-%m-%d")
  Daily <- as.data.frame(matrix(ncol=2,nrow=length(values)))
  colnames(Daily) <- c('date','discharge')
  Daily$discharge <- values
  Daily$date <- dates
  return (Daily)
}

getAllSites <- function(site_url){
  cat(paste("Retrieving data from: \n", site_url, "\n", sep = " "))
  doc<-xmlTreeParse(site_url, getDTD=F, useInternalNodes=TRUE)
  values<-xpathSApply(doc, "//gml:featureMember//nwc:site_no")
  values<-sapply(values, function(x) toString(xmlValue(x)))
  all_sites<-vector(length=length(values))
  all_sites<-values
  return (all_sites)
}

# This function computes the Nash-Sutcliffe value between two data series
nse<-function(timeseries1,timeseries2) {
  numerat<-sum((timeseries1-timeseries2)^2,na.rm=TRUE)
  denomin<-sum((timeseries1-mean(timeseries1,na.rm=TRUE))^2,na.rm=TRUE)  #6/18/11: NSE value calculation has been fixed
  nse<-(1-(numerat/denomin))
  return(nse)
}

# This function computes the Nash-Sutcliffe value for the natural
# logarithms of the two timeseries. Zeros are removed from the data series first.
nselog<-function(timeseries1,timeseries2) {
  # Count of zeros in dataset
  sszeros<-subset(timeseries1,timeseries1==0)
  czeros<-length(sszeros)
  
  # Put timeseries1 and timeseries2 into a data frame  and add header
  obsestq<-data.frame(timeseries1,timeseries2)
  colnames(obsestq)<-c("obs","est")
  #attach(obsestq)
  
  # If zeroes in timeseries1, display message and delete zeroes
  if (czeros>0) {
    cat("\n", czeros, "streamflows with a zero value were detected in this dataset. \nThese values will be removed before computing the \nNash-Sutcliffe efficiency value from the natural logs \nof the streamflows.")
  } else {} #Do nothing if no zeros
  nozeros<-subset(obsestq,obsestq$obs>0)
  
  # Compute NS
  numerat<-sum((log(nozeros$obs)-log(nozeros$est))^2,na.rm=TRUE)
  denomin<-sum((log(nozeros$obs)-mean(log(nozeros$obs),na.rm=TRUE))^2,na.rm=TRUE)
  nselog<-(1-(numerat/denomin))
  return(nselog)
}

# This function computes the root-mean-square error between two data series
rmse<-function(timeseries1,timeseries2) {
  sqerror<-(timeseries1-timeseries2)^2
  sumsqerr<-sum(sqerror)
  n<-length(timeseries1)
  rmse<-sqrt(sumsqerr/n)
  return(rmse)
}

sdev <- function(x) {
  sdev <- sd(x$discharge,na.rm=TRUE)
  return(sdev)
}

cv <- function(x) {
  x1 <- ma1(x)
  x2 <- sdev(x)
  skew <- x2/x1
  return(skew)
}

monthly.mean.ts <- function(qfiletempf,modsite) {
  meanmonts <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanmonts) <- c("Year","Month","Mean_disch")
  return(meanmonts)
}

l7Q10 <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
    "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$year_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_90<-floor(findrank(length(sort_7day),0.90))
  if (rank_90 > 0) { 
    l7Q10<-sort_7day[rank_90]
  } else { 
    l7Q10<-FALSE 
  }
  return(l7Q10)
}

l7Q2 <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
    "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$year_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_50<-floor(findrank(length(sort_7day),0.50))
  if (rank_50 > 0) { 
    l7Q2<-sort_7day[rank_50] 
  } else { 
    l7Q2<-FALSE 
  }
  return(l7Q2)
}
return_10 <- function(qfiletempf) {
  annual_max <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), max, na.rm=TRUE)
  sort_annual_max <- sort(annual_max$x)
  rank_10 <- floor(findrank(length(sort_annual_max),0.10))
  return_10 <- sort_annual_max[rank_10]
  return(return_10)
}
setwd('/Users/jlthomps/Documents/R/')
#a<-read.csv(header=F,colClasses=c("character"),text=sites)
a<-read.csv("sites_waters_stat_part.txt",header=F,colClasses=c("character"))
#a<-t(getAllSites(site_url))
system("rm monthly*txt")
al<-length(a)
yv<-vector(length=al)
ma1v<-vector(length=al)
ma2v<-vector(length=al)
ma3v<-vector(length=al)
ma4v<-vector(length=al)
ma5v<-vector(length=al)
ma6v<-vector(length=al)
ma7v<-vector(length=al)
ma8v<-vector(length=al)
ma9v<-vector(length=al)
ma10v<-vector(length=al)
ma11v<-vector(length=al)
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
ma24v<-vector(length=al)
ma25v<-vector(length=al)
ma26v<-vector(length=al)
ma27v<-vector(length=al)
ma28v<-vector(length=al)
ma29v<-vector(length=al)
ma30v<-vector(length=al)
ma31v<-vector(length=al)
ma32v<-vector(length=al)
ma33v<-vector(length=al)
ma34v<-vector(length=al)
ma35v<-vector(length=al)
ma36v<-vector(length=al)
ma37v<-vector(length=al)
ma38v<-vector(length=al)
ma39v<-vector(length=al)
ma40v<-vector(length=al)
ma41v<-vector(length=al)
ma42v<-vector(length=al)
ma43v<-vector(length=al)
ma44v<-vector(length=al)
ma45v<-vector(length=al)
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
ml13v<-vector(length=al)
ml14v<-vector(length=al)
ml15v<-vector(length=al)
ml16v<-vector(length=al)
ml17v<-vector(length=al)
ml18v<-vector(length=al)
ml19v<-vector(length=al)
ml20v<-vector(length=al)
ml21v<-vector(length=al)
ml22v<-vector(length=al)
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
mh13v<-vector(length=al)
mh14v<-vector(length=al)
mh15v<-vector(length=al)
mh16v<-vector(length=al)
mh17v<-vector(length=al)
mh18v<-vector(length=al)
mh19v<-vector(length=al)
mh20v<-vector(length=al)
mh21v<-vector(length=al)
mh22v<-vector(length=al)
mh23v<-vector(length=al)
mh24v<-vector(length=al)
mh25v<-vector(length=al)
mh26v<-vector(length=al)
mh27v<-vector(length=al)
fl1v<-vector(length=al)
fl2v<-vector(length=al)
fh1v<-vector(length=al)
fh2v<-vector(length=al)
fh3v<-vector(length=al)
fh4v<-vector(length=al)
dl1v<-vector(length=al)
dl2v<-vector(length=al)
dl3v<-vector(length=al)
dl4v<-vector(length=al)
dl5v<-vector(length=al)
dl6v<-vector(length=al)
dl7v<-vector(length=al)
dl8v<-vector(length=al)
dl9v<-vector(length=al)
dl10v<-vector(length=al)
dl18v<-vector(length=al)
dh1v<-vector(length=al)
dh2v<-vector(length=al)
dh3v<-vector(length=al)
dh4v<-vector(length=al)
dh5v<-vector(length=al)
dh10v<-vector(length=al)
dh11v<-vector(length=al)
tl1v<-vector(length=al)
tl2v<-vector(length=al)
th1v<-vector(length=al)
th2v<-vector(length=al)
ra1v<-vector(length=al)
ra2v<-vector(length=al)
ra3v<-vector(length=al)
ra4v<-vector(length=al)
l7Q10v<-vector(length=al)
l7Q2v<-vector(length=al)
return_10v<-vector(length=al)
sites<-a
yv<-vector(length=al)
ymaxv<-vector(length=al)


comment<-vector(length=al)
mean_flow<-vector(length=al)
med_flow<-vector(length=al)
cv_flow<-vector(length=al)
cv_daily<-vector(length=al)
flow_10_obs<-vector(length=al)
flow_25_obs<-vector(length=al)
flow_50_obs<-vector(length=al)
flow_75_obs<-vector(length=al)
flow_90_obs<-vector(length=al)
#ggofv<-vector(length=al)
dfcvbyyrf_list<-vector(mode="list")


for (i in 1:length(sites)){
startdate<-"1900-01-01"
enddate<-"2012-10-01"
sites=a[i]
url2<-paste(sos_url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,'&access=3',sep='')
x_obs <- getXMLWML1.1Data(url2)
#x_obs <- getXMLDV2Data(sos_url,sites,property,offering,startdate,enddate,interval,latest)
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
monthly_mean<-monthly.mean.ts(obs_data,sites)
write.table(monthly_mean,file=file,col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

meanbyyr <- aggregate(obs_data$discharge, list(obs_data$year_val), 
                      mean, na.rm=TRUE)
colnames(meanbyyr) <- c("Year", "meanq")

  mean_flow[i]<-mean(meanbyyr$meanq,na.rm=TRUE)
  med_flow[i]<-median(meanbyyr$meanq,na.rm=TRUE)
  cv_flow[i]<-sd(meanbyyr$meanq,na.rm=TRUE)/mean(meanbyyr$meanq,na.rm=TRUE)
  cv_daily[i]<-cv(obs_data)
                    ma1v[i]<-ma1(obs_data)
                    ma2v[i]<-ma2(obs_data)
                    ma3v[i]<-ma3(obs_data)
                    ma4v[i]<-unlist(ma4.11(obs_data)[1])
                    ma5v[i]<-unlist(ma4.11(obs_data)[2])
                    ma6v[i]<-unlist(ma4.11(obs_data)[3])
                    ma7v[i]<-unlist(ma4.11(obs_data)[4])
                    ma8v[i]<-unlist(ma4.11(obs_data)[5])
                    ma9v[i]<-unlist(ma4.11(obs_data)[6])
                    ma10v[i]<-unlist(ma4.11(obs_data)[7])
                    ma11v[i]<-unlist(ma4.11(obs_data)[8])
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
                    ma24v[i]<-ma24.35(obs_data)[1,1]
                    ma25v[i]<-ma24.35(obs_data)[2,1]
                    ma26v[i]<-ma24.35(obs_data)[3,1]
                    ma27v[i]<-ma24.35(obs_data)[4,1]
                    ma28v[i]<-ma24.35(obs_data)[5,1]
                    ma29v[i]<-ma24.35(obs_data)[6,1]
                    ma30v[i]<-ma24.35(obs_data)[7,1]
                    ma31v[i]<-ma24.35(obs_data)[8,1]
                    ma32v[i]<-ma24.35(obs_data)[9,1]
                    ma33v[i]<-ma24.35(obs_data)[10,1]
                    ma34v[i]<-ma24.35(obs_data)[11,1]
                    ma35v[i]<-ma24.35(obs_data)[12,1]
                    ma36v[i]<-unlist(ma36.40(obs_data)[1])
                    ma37v[i]<-unlist(ma36.40(obs_data)[2])
                    ma38v[i]<-unlist(ma36.40(obs_data)[3])
                    ma39v[i]<-unlist(ma36.40(obs_data)[4])
                    ma40v[i]<-unlist(ma36.40(obs_data)[5])
                    ma41v[i]<-unlist(ma41.45(obs_data,drain_area)[1])
                    ma42v[i]<-unlist(ma41.45(obs_data,drain_area)[2])
                    ma43v[i]<-unlist(ma41.45(obs_data,drain_area)[3])
                    ma44v[i]<-unlist(ma41.45(obs_data,drain_area)[4])
                    ma45v[i]<-unlist(ma41.45(obs_data,drain_area)[5])
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
                    ml13v[i]<-ml13(obs_data)
                    ml14v[i]<-unlist(ml14.16(obs_data)[1])
                    ml15v[i]<-unlist(ml14.16(obs_data)[2])
                    ml16v[i]<-unlist(ml14.16(obs_data)[3])
                    ml17v[i]<-ml17(obs_data)
                    ml18v[i]<-ml18(obs_data)
                    ml19v[i]<-ml19(obs_data)
                    ml20v[i]<-ml20(obs_data)
                    ml21v[i]<-ml21(obs_data)
                    ml22v[i]<-ml22(obs_data,drain_area)
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
                    mh13v[i]<-mh13(obs_data)
                    mh14v[i]<-mh14(obs_data)
                    mh15v[i]<-unlist(mh15.17(obs_data)[1])
                    mh16v[i]<-unlist(mh15.17(obs_data)[2])
                    mh17v[i]<-unlist(mh15.17(obs_data)[3])
                    mh18v[i]<-mh18(obs_data)
                    mh19v[i]<-mh19(obs_data)
                    mh20v[i]<-mh20(obs_data,drain_area)
                    mh21v[i]<-mh21(obs_data)
                    mh22v[i]<-mh22(obs_data)
                    mh23v[i]<-mh23(obs_data)
                    mh24v[i]<-mh24(obs_data)
                    mh25v[i]<-mh25(obs_data)
                    mh26v[i]<-mh26(obs_data)
                    mh27v[i]<-mh27(obs_data)
                    fl1v[i]<-unlist(fl1.2(obs_data)[1])
                    fl2v[i]<-unlist(fl1.2(obs_data)[2])
                    fh1v[i]<-unlist(fh1.2(obs_data)[1])
                    fh2v[i]<-unlist(fh1.2(obs_data)[2])
                    fh3v[i]<-fh3(obs_data)
                    fh4v[i]<-fh4(obs_data)
                    dl1v[i]<-dl1(obs_data)
                    dl2v[i]<-dl2(obs_data)
                    dl3v[i]<-dl3(obs_data)
                    dl4v[i]<-dl4(obs_data)
                    dl5v[i]<-dl5(obs_data)
                    dl6v[i]<-dl6(obs_data)
                    dl7v[i]<-dl7(obs_data)
                    dl8v[i]<-dl8(obs_data)
                    dl9v[i]<-dl9(obs_data)
                    dl10v[i]<-dl10(obs_data)
                    dl18v[i]<-dl18(obs_data)
                    dh1v[i]<-dh1(obs_data)
                    dh2v[i]<-dh2(obs_data)
                    dh3v[i]<-dh3(obs_data)
                    dh4v[i]<-dh4(obs_data)
                    dh5v[i]<-dh5(obs_data)
                    dh10v[i]<-dh10(obs_data)
                    dh11v[i]<-dh11(obs_data)
                    tl1v[i]<-unlist(tl1.2(obs_data)[1])
                    tl2v[i]<-unlist(tl1.2(obs_data)[2])
                    th1v[i]<-unlist(th1.2(obs_data)[1])
                    th2v[i]<-unlist(th1.2(obs_data)[2])
                    ra1v[i]<-ra1(obs_data)
                    ra2v[i]<-ra2(obs_data)
                    ra3v[i]<-ra3(obs_data)
                    ra4v[i]<-ra4(obs_data)
                    l7Q10v[i]<-l7Q10(obs_data)
                    l7Q2v[i]<-l7Q2(obs_data)
                    return_10v[i]<-return_10(obs_data)
comment[i]<-""

sort_x_obs<-sort(obs_data$discharge)
rank_10obs<-floor(findrank(length(sort_x_obs),0.10))
rank_25obs<-floor(findrank(length(sort_x_obs),0.25))
rank_50obs<-floor(findrank(length(sort_x_obs),0.5))
rank_75obs<-floor(findrank(length(sort_x_obs),0.75))
rank_90obs<-floor(findrank(length(sort_x_obs),0.9))
flow_10_obs[i]<-sort_x_obs[rank_10obs]
flow_25_obs[i]<-sort_x_obs[rank_25obs]
flow_50_obs[i]<-sort_x_obs[rank_50obs]
flow_75_obs[i]<-sort_x_obs[rank_75obs]
flow_90_obs[i]<-sort_x_obs[rank_90obs]
}
} else {
  comment[i]<-"No observed data for this site"
}
}


statsout<-data.frame(t(a),yv,ymaxv,mean_flow,med_flow,cv_flow,
                     flow_10_obs,flow_25_obs,flow_50_obs,flow_75_obs,flow_90_obs,
                     cv_daily,ma1v,ma2v,ma3v,ma4v,ma5v,ma6v,ma7v,ma8v,ma9v,ma10v,ma11v,ma12v,ma13v,
                     ma14v,ma15v,ma16v,ma17v,ma18v,ma19v,ma20v,
                     ma21v,ma22v,ma23v,ma24v,ma25v,ma26v,ma27v,
                     ma28v,ma29v,ma30v,ma31v,ma32v,ma33v,ma34v,ma35v,ma36v,
                     ma37v,ma38v,ma39v,ma40v,ma41v,ma42v,ma43v,ma44v,ma45v,ml1v,ml2v,ml3v,ml4v,ml5v,ml6v,ml7v,ml8v,ml9v,
                     ml10v,ml11v,ml12v,ml13v,ml14v,ml15v,ml16v,ml17v,ml18v,ml19v,ml20v,ml21v,ml22v,mh1v,mh2v,mh3v,mh4v,mh5v,
                     mh6v,mh7v,mh8v,mh9v,mh10v,mh11v,mh12v,mh13v,mh14v,mh15v,mh16v,mh17v,mh18v,mh19v,mh20v,mh21v,
                     mh22v,mh23v,mh24v,mh25v,mh26v,mh27v,
                     fl1v,fl2v,fh1v,fh2v,fh3v,
                     fh4v,dl1v,dl2v,dl3v,dl4v,dl5v,
                     dl6v,dl7v,dl8v,dl9v,dl10v,dl18v,dh1v,dh2v,dh3v,dh4v,dh5v,
                     dh10v,dh11v,tl1v,tl2v,th1v,th2v,
                     ra1v,ra2v,ra3v,ra4v,l7Q10v,l7Q2v,return_10v,
                     comment)
colnames(statsout)<-c('site_no','min_date','max_date','mean_of_annual_flows','median_of_annual_flows','cv_of_annual_flows',
                      'flow_10_obs,','flow_25_obs','flow_50_obs','flow_75_obs','flow_90_obs',
                      'cv_daily_flows','ma1_mean_disc','ma2_median_disc','ma3_mean_annual_var','ma4','ma5_skew','ma6','ma7','ma8','ma9','ma10','ma11','ma12_jan_mean','ma13_feb_mean',
                      'ma14_mar_mean','ma15_apr_mean','ma16_may_mean','ma17_june_mean','ma18_july_mean','ma19_aug_mean','ma20_sep_mean',
                      'ma21_oct_mean','ma22_nov_mean','ma23_dec_mean','ma24_jan_var','ma25_feb_var','ma26_mar_var','ma27_apr_var',
                      'ma28_may_var','ma29_jun_var','ma30_july_var','ma31_aug_var','ma32_sep_var','ma33_oct_var','ma34_nov_var','ma35_dec_var','ma36',
                      'ma37_var_across_months','ma38','ma39_monthly_std_dev','ma40_monthly_skewness','ma41','ma42','ma43','ma44','ma45','ml1','ml2','ml3','ml4','ml5','ml6','ml7','ml8','ml9',
                      'ml10','ml11','ml12','ml13_min_monthly_var','ml14_min_annual_flow','ml15','ml16','ml17','ml18','ml19','ml20','ml21','ml22',
                      'mh1','mh2','mh3','mh4','mh5','mh6','mh7','mh8','mh9','mh10','mh11','mh12','mh13','mh14_med_annual_max',
                      'mh15','mh16_high_flow_index','mh17','mh18','mh19','mh20','mh21','mh22','mh23','mh24','mh25','mh26_high_peak_flow','mh27',
                      'fl1_low_flood_pulse','fl2_low_pulse_var','fh1_high_pulse_count','fh2_high_pulse_var','fh3_high_pulse_count_three',
                      'fh4_high_pulse_count_seven','dl1_min_daily_flow','dl2_min_3_day_avg','dl3','dl4_min_30_day_avg','dl5_min_90_day_avg',
                      'dl6_min_flow_var','dl7','dl8','dl9_min_30_day_var','dl10_min_90_day_var','dl18_zero_flow_days','dh1','dh2','dh3','dh4','dh5_max_90_day_avg',
                      'dh10_max_90_day_var','dh11','tl1_min_flow_julian_day','tl2_min_julian_var','th1_max_flow_julian_day','th2_max_julian_var',
                      'ra1_rise_rate','ra2_rise_rate_var','ra3_fall_rate','ra4_fall_rate_var','7Q10_obs','7Q2_obs','10_year_return_max_obs',
                      'comment')
output="output.txt"
write.table(statsout,file="output.txt",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
