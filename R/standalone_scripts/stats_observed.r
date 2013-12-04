# wps.des: id=test_stats, title = test stats, abstract = Finds the mean daily flow median daily flow and skewness of daily flow in the input dataset;
# wps.in: sites, string, list of sites, A list of sites;

library(XML)
library(zoo)
library(chron)
library(doBy)
library(hydroGOF)
#library(dataRetrieval)

sos_url_temp="http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering_temp='00003'
property_temp='00060'

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

getXMLWML1.1Data <- function(obs_url){
  cat(paste("Retrieving data from: \n", obs_url, "\n", sep = " "))
  doc<-xmlTreeParse(obs_url, getDTD=F, useInternalNodes=TRUE)
  values<-xpathSApply(doc, "//ns1:timeSeries//ns1:value")
  values2<-sapply(values,function(x) as.numeric(xmlValue(x)))
  dateSet<-xpathSApply(doc, "//@dateTime")
  dateSet2<-sapply(dateSet,function(x) toString(substr(x,1,10)))
  Daily<-as.data.frame(matrix(ncol=2,nrow=length(values2)))
  colnames(Daily)<-c('date','discharge')
  Daily$discharge<-values2
  if (length(dateSet)>2) {
    Daily$date<-dateSet}
  return (Daily)
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
  sszeros<<-subset(timeseries1,timeseries1==0)
  czeros<<-length(sszeros)
  
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

medflowbyyear <- function(qfiletempf) {
  aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
            median, na.rm=TRUE)
}

meanflowbyyear <- function(qfiletempf) {
  aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
            mean, na.rm=TRUE)
}

bfi <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(rollingavg, rollingavg$day7mean != 
    "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$year_val), min)
  meanflow <- meanflowbyyear(qfiletempf)
  compbfi <- merge(min7daybyyear, meanflow, by = "Group.1")
  colnames(compbfi) <- c("year", "day7min", "meandaily")
  bfi <- compbfi$day7min/compbfi$meandaily
}

findrank <- function(n, p) {
  r <- (1 - p) * (n + 1)
  findrank <- floor(r)
}

ma1 <- function(x) {
  ma1 <- mean(x$discharge,na.rm=TRUE)
}

ma2 <- function(x) {
  ma2 <- median(x$discharge,na.rm=TRUE)
}

sdev <- function(x) {
  sdev <- sd(x$discharge,na.rm=TRUE)
}

ma3 <- function(qfiletempf, pref = "mean") {
  sdbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                      FUN = sd, na.rm=TRUE)
  colnames(sdbyyr) <- c("Year", "sdq")
  meanbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                        mean, na.rm=TRUE)
  colnames(meanbyyr) <- c("Year", "meanq")
  dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
                         meanbyyr$meanq)
  colnames(dfcvbyyr) <- c("Year", "sdq", "meanq")
  cvbyyr <- dfcvbyyr$sdq/dfcvbyyr$meanq
  dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr)
  colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", 
                           "cvq")
  if (pref == "median") {
    medcv <- median(dfcvbyyrf$cvq, na.rm=TRUE)
    ma3 <- (medcv) * 100
  }
  else {
    meancv <- mean(dfcvbyyrf$cvq, na.rm=TRUE)
    ma3 <- (meancv) * 100
  }
}

ma5 <- function(x) {
  x1 <- ma1(x)
  x2 <- ma2(x)
  ma5 <- x1/x2
}

cv <- function(x) {
  x1 <- ma1(x)
  x2 <- sdev(x)
  skew <- x2/x1
}

ma12.23 <- function(qfiletempf, pref = "mean") {
  if (pref == "median") {
    medmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
                        median, na.rm=TRUE)
    ma12.23 <- data.frame(medmon)
  }
  else {
    meanmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
                         mean, na.rm=TRUE)
    ma12.23 <- data.frame(meanmon)
  }
}

mamin12.23 <- function(qfiletempf) {
  minmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
                      min, na.rm=TRUE)
  mamin12.23 <- data.frame(minmon)
}

mamax12.23 <- function(qfiletempf) {
  maxmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
                      max, na.rm=TRUE)
  mamax12.23 <- data.frame(maxmon)
}

monthly.mean.ts <- function(qfiletempf,modsite) {
  meanmonts <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanmonts) <- c("Year","Month","Mean_disch")
  return(meanmonts)
}

ma24.35 <- function(qfiletempf, pref = "mean") {
  sdmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                    qfiletempf$month_val), FUN = sd, na.rm=TRUE)
  colnames(sdmonbyyr) <- c("Year", "Month", "sdq")
  meanmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                      qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanmonbyyr) <- c("Year", "Month", "meanq")
  dfcvmonbyyr <- data.frame(meanmonbyyr$Year, meanmonbyyr$Month, 
                            sdmonbyyr$sdq, meanmonbyyr$meanq)
  colnames(dfcvmonbyyr) <- c("Year", "Month", "sdq", 
                             "meanq")
  cvmonbyyr <- dfcvmonbyyr$sdq/dfcvmonbyyr$meanq
  dfcvmonbyyrf <- data.frame(dfcvmonbyyr, cvmonbyyr)
  colnames(dfcvmonbyyrf) <- c("Year", "Month", "sdq", 
                              "meanq", "cvq")
  if (pref == "median") {
    medmoncv <- aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), 
                          median, na.rm=TRUE)
    ma24.35 <- data.frame(medmoncv[2] * 100)
  }
  else {
    meanmoncv <- aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), 
                           mean, na.rm=TRUE)
    ma24.35 <- data.frame(meanmoncv[2] * 100)
  }
}

ma37 <- function(qfiletempf) {
  sdmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                    qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(sdmonbyyr) <- c("Year", "Month", "meanmo")
  quart <- summary(sdmonbyyr$meanmo)
  thirdquart <- quart[5]
  firstquart <- quart[2]
  diffquart <- thirdquart - firstquart
  medianquart <- quart[3]
  ma37 <- diffquart[1]/medianquart[1]
}

ma39 <- function(qfiletempf) {
  sdmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                    qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(sdmonbyyr) <- c("Year", "Month", "meanmo")
  sdmonflows <- sd(sdmonbyyr$meanmo)
  meanmonflows <- mean(sdmonbyyr$meanmo)
  ma39 <- (sdmonflows * 100)/meanmonflows
}

ma40 <- function(qfiletempf) {
  numer1 <- colMeans(ma12.23(qfiletempf)[2])
  numer2 <- apply(ma12.23(qfiletempf, pref = "median")[2], 
                  2, median)
  ma40 <- (numer1 - numer2)/numer2
}

ml13 <- function(qfiletempf) {
  minmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                     qfiletempf$month_val), FUN = min, na.rm=TRUE)
  colnames(minmonbyyr) <- c("Year", "Month", "minmo")
  sdminmonflows <- sd(minmonbyyr$minmo)
  meanminmonflows <- mean(minmonbyyr$minmo)
  ml13 <- (sdminmonflows * 100)/meanminmonflows
}

ml14 <- function(qfiletempf) {
  min1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$year_val), min, na.rm=TRUE)
  medflow <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       median, na.rm=TRUE)
  computeml14 <- merge(min1daybyyear, medflow, by = "Group.1")
  colnames(computeml14) <- c("year", "day1min", "meddaily")
  dfml14 <- computeml14$day1min/computeml14$meddaily
  ml14 <- mean(dfml14)
}

ml17 <- function(qfiletempf, pref = "mean") {
  bfibyyear <- bfi(qfiletempf)
  if (pref == "median") {
    ml17 <- median(bfibyyear)
  }
  else {
    ml17 <- mean(bfibyyear)
  }
}

ml18 <- function(qfiletempf) {
  bfibyyear <- bfi(qfiletempf)
  sdbfi <- sd(bfibyyear)
  meanbfi <- mean(bfibyyear)
  ml18 <- meanbfi/sdbfi
}

mh14 <- function(qfiletempf) {
  maxmonbymoyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                       qfiletempf$month_val), FUN = max, na.rm=TRUE)
  colnames(maxmonbymoyr) <- c("Year", "Month", "momax")
  maxmonbyyrr <- aggregate(maxmonbymoyr$momax, list(maxmonbymoyr$Year), 
                           FUN = max, na.rm=TRUE)
  colnames(maxmonbyyrr) <- c("Year", "yrmax")
  medflowbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                           FUN = median, na.rm=TRUE)
  colnames(medflowbyyr) <- c("Year", "yrmed")
  ratiomaxmed <- maxmonbyyrr$yrmax/medflowbyyr$yrmed
  mh14 <- median(ratiomaxmed)
}

mh16 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.1))
  hfcrit <- sortq[frank]
  mh16 <- hfcrit/ma2(qfiletempf)
}

mh26 <- function(qfiletempf) {
  hfcrit <- 7 * ma2(qfiletempf)
  isolateq <- qfiletempf$discharge
  exchfcrit <- subset(isolateq, isolateq > hfcrit)
  meanex <- mean(exchfcrit)
  mh26 <- meanex/ma2(qfiletempf)
}

fl1 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.75))
  lfcrit <- sortq[frank]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  lfcountbyyr <- rep(0, noyrs)
  counter <- 0
  for (i in as.character(noyears$Year[1]):as.character(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
      i)
    echfcrit <- subset(subsetyr, subsetyr$discharge < 
      lfcrit)
    counter <- counter + 1
    lfcountbyyr[counter] <- length(echfcrit$discharge)
  }
  lfcntbyyr <<- lfcountbyyr
  if (pref == "median") {
    fl1 <- median(lfcntbyyr)
  }
  else {
    fl1 <- mean(lfcntbyyr)
  }
}

fl2 <- function(qfiletempf) {
  meanfl1 <- fl1(qfiletempf, pref = "mean")
  stdevfl1 <- sd(lfcntbyyr)
  fl2 <- (stdevfl1 * 100)/meanfl1
}

fh1 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.25))
  hfcrit <- sortq[frank]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyr <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
      i)
    echfcrit <- subset(subsetyr, subsetyr$discharge > 
      hfcrit)
    counter <- counter + 1
    hfcountbyyr[counter] <- length(echfcrit$discharge)
  }
  hfcntbyyr <<- hfcountbyyr
  if (pref == "median") {
    fh1 <- median(hfcntbyyr)
  }
  else {
    fh1 <- mean(hfcntbyyr)
  }
}

fh2 <- function(qfiletempf) {
  meanfh1 <- fh1(qfiletempf, pref = "mean")
  stdevfl1 <- sd(hfcntbyyr)
  fh2 <- (stdevfl1 * 100)/meanfh1
}

fh3 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 3 * ma2(qfiletempf)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh3 <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
      i)
    echfcrit <- subset(subsetyr, subsetyr$discharge > 
      hfcrit)
    counter <- counter + 1
    hfcountbyyrfh3[counter] <- length(echfcrit$discharge)
  }
  hfcntbyyrfh3 <<- hfcountbyyrfh3
  if (pref == "median") {
    fh3 <- median(hfcntbyyrfh3)
  }
  else {
    fh3 <- mean(hfcntbyyrfh3)
  }
}

fh4 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 7 * ma2(qfiletempf)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh4 <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
      i)
    echfcrit <- subset(subsetyr, subsetyr$discharge > 
      hfcrit)
    counter <- counter + 1
    hfcountbyyrfh4[counter] <- length(echfcrit$discharge)
  }
  hfcntbyyrfh4 <<- hfcountbyyrfh4
  if (pref == "median") {
    fh4 <- median(hfcntbyyrfh4)
  }
  else {
    fh4 <- mean(hfcntbyyrfh4)
  }
}

dl1 <- function(qfiletempf, pref = "mean") {
  day1mean <- rollmean(qfiletempf$discharge, 1, align = "right", 
                       na.pad = TRUE)
  day1rollingavg <- data.frame(qfiletempf, day1mean)
  rollingavgs1day <- subset(day1rollingavg, day1rollingavg$day1mean != 
    "NA")
  min1daybyyear <<- aggregate(rollingavgs1day$day1mean, 
                              list(rollingavgs1day$year_val), min, na.rm=TRUE)
  if (pref == "median") {
    dl1 <- median(min1daybyyear$x)
  }
  else {
    dl1 <- mean(min1daybyyear$x)
  }
}

dl2 <- function(qfiletempf, pref = "mean") {
  day3mean <- rollmean(qfiletempf$discharge, 3, align = "right", 
                       na.pad = TRUE)
  day3rollingavg <- data.frame(qfiletempf, day3mean)
  rollingavgs3day <- subset(day3rollingavg, day3rollingavg$day3mean != 
    "NA")
  min3daybyyear <<- aggregate(rollingavgs3day$day3mean, 
                              list(rollingavgs3day$year_val), min, na.rm=TRUE)
  if (pref == "median") {
    dl2 <- median(min3daybyyear$x)
  }
  else {
    dl2 <- mean(min3daybyyear$x)
  }
}

dl4 <- function(qfiletempf, pref = "mean") {
  day30mean <- rollmean(qfiletempf$discharge, 30, align = "right", 
                        na.pad = TRUE)
  day30rollingavg <- data.frame(qfiletempf, day30mean)
  rollingavgs30day <- subset(day30rollingavg, day30rollingavg$day30mean != 
    "NA")
  min30daybyyear <<- aggregate(rollingavgs30day$day30mean, 
                               list(rollingavgs30day$year_val), min, na.rm=TRUE)
  if (pref == "median") {
    dl5 <- median(min30daybyyear$x)
  }
  else {
    dl5 <- mean(min30daybyyear$x)
  }
}

dl5 <- function(qfiletempf, pref = "mean") {
  day90mean <- rollmean(qfiletempf$discharge, 90, align = "right", 
                        na.pad = TRUE)
  day90rollingavg <- data.frame(qfiletempf, day90mean)
  rollingavgs90day <- subset(day90rollingavg, day90rollingavg$day90mean != 
    "NA")
  min90daybyyear <<- aggregate(rollingavgs90day$day90mean, 
                               list(rollingavgs90day$year_val), min, na.rm=TRUE)
  if (pref == "median") {
    dl5 <- median(min90daybyyear$x)
  }
  else {
    dl5 <- mean(min90daybyyear$x)
  }
}

dl6 <- function(qfiletempf) {
  meandl6 <- dl1(qfiletempf, pref = "mean")
  sddl6 <- sd(min1daybyyear$x)
  dl6 <- (sddl6 * 100)/meandl6
}

dl9 <- function(qfiletempf) {
  meandl9 <- dl4(qfiletempf, pref = "mean")
  sddl9 <- sd(min30daybyyear$x)
  dl9 <- (sddl9 * 100)/meandl9
}

dl10 <- function(qfiletempf) {
  meandl10 <- dl5(qfiletempf, pref = "mean")
  sddl10 <- sd(min90daybyyear$x)
  dl10 <- (sddl10 * 100)/meandl10
}

dl18 <- function(qfiletempf, pref = "mean") {
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountzeros <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, qfiletempf$year_val == 
      i)
    echfcrit <- subset(subsetyr, subsetyr$discharge < 
      0.001)
    counter <- counter + 1
    hfcountzeros[counter] <- length(echfcrit$discharge)
  }
  hfcntzeros <<- hfcountzeros
  if (pref == "median") {
    dl18 <- median(hfcntzeros)
  }
  else {
    dl18 <- mean(hfcntzeros)
  }
}

dh5 <- function(qfiletempf, pref = "mean") {
  day90mean <- rollmean(qfiletempf$discharge, 90, align = "right", 
                        na.pad = TRUE)
  day90rollingavg <- data.frame(qfiletempf, day90mean)
  rollingavgs90day <- subset(day90rollingavg, day90rollingavg$day90mean != 
    "NA")
  max90daybyyear <<- aggregate(rollingavgs90day$day90mean, 
                               list(rollingavgs90day$year_val), max, na.rm=TRUE)
  if (pref == "median") {
    dh5 <- median(max90daybyyear$x)
  }
  else {
    dh5 <- mean(max90daybyyear$x)
  }
}

dh10 <- function(qfiletempf) {
  meandh10 <- dh5(qfiletempf, pref = "mean")
  sddh10 <- sd(max90daybyyear$x)
  dh10 <- (sddh10 * 100)/meandh10
}

tl1 <- function(qfiletempf, pref = "mean") {
  min1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$year_val), min, na.rm=TRUE)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momin")
  noyrs <- length(noyears$Year)
  juldaymin <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    counter <- counter + 1
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
      i)
    findjulday <- subset(subsetyr, subsetyr$discharge == 
      min1daybyyear$x[counter])
    juldaymin[counter] <- findjulday$jul_val[1]
  }
  minjulday <<- juldaymin
  if (pref == "median") {
    tl1 <- median(minjulday)
  }
  else {
    tl1 <- mean(minjulday)
  }
}

tl2 <- function(qfiletempf) {
  meantl2 <- tl1(qfiletempf, pref = "mean")
  sddtl2 <- sd(minjulday)
  tl2 <- (sddtl2 * 100)/meantl2
}

th1 <- function(qfiletempf, pref = "mean") {
  max1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$year_val), max, na.rm=TRUE)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  juldaymax <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    counter <- counter + 1
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
      i)
    findjulday <- subset(subsetyr, subsetyr$discharge == 
      max1daybyyear$x[counter])
    juldaymax[counter] <- findjulday$jul_val[1]
  }
  maxjulday <<- juldaymax
  if (pref == "median") {
    th1 <- median(maxjulday)
  }
  else {
    th1 <- mean(maxjulday)
  }
}

th2 <- function(qfiletempf) {
  meanth2 <- th1(qfiletempf, pref = "mean")
  sddth2 <- sd(maxjulday)
  th2 <- (sddth2 * 100)/meanth2
}

ra1 <- function(qfiletempf, pref = "mean") {
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findrisevalues <<- subset(diffbtdays, diffbtdays > 
    0)
  if (pref == "median") {
    ra1 <- median(findrisevalues)
  }
  else {
    ra1 <- mean(findrisevalues)
  }
}

ra3 <- function(qfiletempf, pref = "mean") {
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findfallvalueneg <- subset(diffbtdays, diffbtdays < 
    0)
  findfallvalues <<- abs(findfallvalueneg)
  if (pref == "median") {
    ra3 <- median(findfallvalues)
  }
  else {
    ra3 <- mean(findfallvalues)
  }
}

ra4 <- function(qfiletempf) {
  meanra4 <- ra3(qfiletempf, pref = "mean")
  sddra4 <- sd(findfallvalues)
  ra4 <- (sddra4 * 100)/meanra4
}

l7Q10 <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
    "NA")
  min7daybyyear <<- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$year_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_90<-floor(findrank(length(sort_7day),0.90))
  if (rank_90 > 0) { 
    l7Q10<-sort_7day[rank_90]
  } else { 
    l7Q10<-FALSE 
  }
}

l7Q2 <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
    "NA")
  min7daybyyear <<- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$year_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_50<-floor(findrank(length(sort_7day),0.50))
  if (rank_50 > 0) { 
    l7Q2<-sort_7day[rank_50] 
  } else { 
    l7Q2<-FALSE 
  }
}
return_10 <- function(qfiletempf) {
  annual_max <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), max, na.rm=TRUE)
  sort_annual_max <- sort(annual_max$x)
  rank_10 <- floor(findrank(length(sort_annual_max),0.10))
  return_10 <- sort_annual_max[rank_10]
}
setwd('/Users/jlthomps/Documents/R/')
system("del monthly*txt")
#a<-read.csv(header=F,colClasses=c("character"),text=sites)
a<-read.csv("sites_waters_stat.txt",header=F,colClasses=c("character"))
#a<-t(getAllSites(site_url))
al<-length(a)
yv<-vector(length=al)
ma1v<-vector(length=al)
ma2v<-vector(length=al)
ma3v<-vector(length=al)
ma5v<-vector(length=al)
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
ma37v<-vector(length=al)
ma39v<-vector(length=al)
ma40v<-vector(length=al)
ml13v<-vector(length=al)
ml14v<-vector(length=al)
mh14v<-vector(length=al)
mh16v<-vector(length=al)
mh26v<-vector(length=al)
ml17v<-vector(length=al)
ml18v<-vector(length=al)
fl1v<-vector(length=al)
fl2v<-vector(length=al)
fh1v<-vector(length=al)
fh2v<-vector(length=al)
fh3v<-vector(length=al)
fh4v<-vector(length=al)
dl1v<-vector(length=al)
dl2v<-vector(length=al)
dl4v<-vector(length=al)
dl5v<-vector(length=al)
dl6v<-vector(length=al)
dl9v<-vector(length=al)
dl10v<-vector(length=al)
dl18v<-vector(length=al)
dh5v<-vector(length=al)
dh10v<-vector(length=al)
tl1v<-vector(length=al)
tl2v<-vector(length=al)
th1v<-vector(length=al)
th2v<-vector(length=al)
ra1v<-vector(length=al)
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
mamin12v<-vector(length=al)
mamin13v<-vector(length=al)
mamin14v<-vector(length=al)
mamin14v<-vector(length=al)
mamin15v<-vector(length=al)
mamin16v<-vector(length=al)
mamin17v<-vector(length=al)
mamin18v<-vector(length=al)
mamin19v<-vector(length=al)
mamin20v<-vector(length=al)
mamin21v<-vector(length=al)
mamin22v<-vector(length=al)
mamin23v<-vector(length=al)
mamax12v<-vector(length=al)
mamax13v<-vector(length=al)
mamax14v<-vector(length=al)
mamax14v<-vector(length=al)
mamax15v<-vector(length=al)
mamax16v<-vector(length=al)
mamax17v<-vector(length=al)
mamax18v<-vector(length=al)
mamax19v<-vector(length=al)
mamax20v<-vector(length=al)
mamax21v<-vector(length=al)
mamax22v<-vector(length=al)
mamax23v<-vector(length=al)
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
url2<-paste(sos_url_temp,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering_temp,'&parameterCd=',property_temp,'&access=3',sep='')
x_obs <- getXMLWML1.1Data(url2)
#x_obs <- getXMLDV2Data(sos_url,sites,property,offering,startdate,enddate,interval,latest)
if (nrow(x_obs)>2) {
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
qfiletempf$year_val<-substr(x_obs$date,3,4)
as.numeric(qfiletempf$year_val)
qfiletempf$day_val<-substr(x_obs$date,9,10)
as.numeric(qfiletempf$day_val)
qfiletempf$jul_val<-strptime(x_obs$date, "%Y-%m-%d")$yday+1
as.numeric(qfiletempf$jul_val)
qfiletempf$wy_val<-ifelse(as.numeric(qfiletempf$month_val)>=10,as.character(as.numeric(qfiletempf$year_val)+ones_val),qfiletempf$year_val) 

countbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), length)
colnames(countbyyr)<-c('wy','num_samples')
sub_countbyyr<-subset(countbyyr,num_samples >= 365)
obs_data<-merge(qfiletempf,sub_countbyyr,by.x="wy_val",by.y="wy")
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
sdbyyr <- aggregate(obs_data$discharge, list(obs_data$year_val), 
                    sd)
colnames(sdbyyr) <- c("Year", "sdq")
meanbyyr <- aggregate(obs_data$discharge, list(obs_data$year_val), 
                      mean, na.rm=TRUE)
colnames(meanbyyr) <- c("Year", "meanq")
medbyyr <- aggregate(obs_data$discharge, list(obs_data$year_val), 
                      median, na.rm=TRUE)
colnames(medbyyr) <- c("Year","medq")
dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
                       meanbyyr$meanq, medbyyr$medq)
colnames(dfcvbyyr) <- c("Year", "sdq", "meanq", "medq")
cvbyyr <- dfcvbyyr$sdq/dfcvbyyr$meanq
dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr)
colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", "medq", 
                         "cvq")
dfcvbyyrf_list[[as.character(sites)]]<-dfcvbyyrf

  mean_flow[i]<-mean(dfcvbyyrf$meanq,na.rm=TRUE)
  med_flow[i]<-median(dfcvbyyrf$meanq,na.rm=TRUE)
  cv_flow[i]<-sd(dfcvbyyrf$meanq,na.rm=TRUE)/mean(dfcvbyyrf$meanq,na.rm=TRUE)
  cv_daily[i]<-cv(obs_data)
                    ma1v[i]<-ma1(obs_data)
                    ma2v[i]<-ma2(obs_data)
                    ma3v[i]<-ma3(obs_data)
                    ma5v[i]<-ma5(obs_data)
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
                    ma37v[i]<-unname(ma37(obs_data))
                    ma39v[i]<-ma39(obs_data)
                    ma40v[i]<-unname(ma40(obs_data))
                    ml13v[i]<-ml13(obs_data)
                    ml14v[i]<-ml14(obs_data)
                    ml17v[i]<-ml14(obs_data)
                    ml18v[i]<-ml18(obs_data)
                    mh14v[i]<-mh14(obs_data)
                    mh16v[i]<-mh16(obs_data)
                    mh26v[i]<-mh26(obs_data)
                    fl1v[i]<-fl1(obs_data)
                    fl2v[i]<-fl2(obs_data)
                    fh1v[i]<-fh1(obs_data)
                    fh2v[i]<-fh2(obs_data)
                    fh3v[i]<-fh3(obs_data)
                    fh4v[i]<-fh4(obs_data)
                    dl1v[i]<-dl1(obs_data)
                    dl2v[i]<-dl2(obs_data)
                    dl4v[i]<-dl4(obs_data)
                    dl5v[i]<-dl5(obs_data)
                    dl6v[i]<-dl6(obs_data)
                    dl9v[i]<-dl9(obs_data)
                    dl10v[i]<-dl10(obs_data)
                    dl18v[i]<-dl18(obs_data)
                    dh5v[i]<-dh5(obs_data)
                    dh10v[i]<-dh10(obs_data)
                    tl1v[i]<-tl1(obs_data)
                    tl2v[i]<-tl2(obs_data)
                    th1v[i]<-th1(obs_data)
                    th2v[i]<-th2(obs_data)
                    ra1v[i]<-ra1(obs_data)
                    ra3v[i]<-ra3(obs_data)
                    ra4v[i]<-ra4(obs_data)
                    l7Q10v[i]<-l7Q10(obs_data)
                    l7Q2v[i]<-l7Q2(obs_data)
                    return_10v[i]<-return_10(obs_data)
                    mamin12v[i]<-mamax12.23(obs_data)[1:1,2:2]
                    mamin13v[i]<-mamax12.23(obs_data)[2:2,2:2]
                    mamin14v[i]<-mamax12.23(obs_data)[3:3,2:2]
                    mamin15v[i]<-mamax12.23(obs_data)[4:4,2:2]
                    mamin16v[i]<-mamax12.23(obs_data)[5:5,2:2]
                    mamin17v[i]<-mamax12.23(obs_data)[6:6,2:2]
                    mamin18v[i]<-mamax12.23(obs_data)[7:7,2:2]
                    mamin19v[i]<-mamax12.23(obs_data)[8:8,2:2]
                    mamin20v[i]<-mamax12.23(obs_data)[9:9,2:2]
                    mamin21v[i]<-mamax12.23(obs_data)[10:10,2:2]
                    mamin22v[i]<-mamax12.23(obs_data)[11:11,2:2]
                    mamin23v[i]<-mamax12.23(obs_data)[12:12,2:2]
                    mamax12v[i]<-mamax12.23(obs_data)[1:1,2:2]
                    mamax13v[i]<-mamax12.23(obs_data)[2:2,2:2]
                    mamax14v[i]<-mamax12.23(obs_data)[3:3,2:2]
                    mamax15v[i]<-mamax12.23(obs_data)[4:4,2:2]
                    mamax16v[i]<-mamax12.23(obs_data)[5:5,2:2]
                    mamax17v[i]<-mamax12.23(obs_data)[6:6,2:2]
                    mamax18v[i]<-mamax12.23(obs_data)[7:7,2:2]
                    mamax19v[i]<-mamax12.23(obs_data)[8:8,2:2]
                    mamax20v[i]<-mamax12.23(obs_data)[9:9,2:2]
                    mamax21v[i]<-mamax12.23(obs_data)[10:10,2:2]
                    mamax22v[i]<-mamax12.23(obs_data)[11:11,2:2]
                    mamax23v[i]<-mamax12.23(obs_data)[12:12,2:2]
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
                     cv_daily,ma1v,ma2v,ma3v,ma5v,ma12v,ma13v,
                     ma14v,ma15v,ma16v,ma17v,ma18v,ma19v,ma20v,
                     ma21v,ma22v,ma23v,mamin12v,mamin13v,mamin14v,mamin15v,mamin16v,mamin17v,mamin18v,
                     mamin19v,mamin20v,mamin21v,mamin22v,mamin23v,mamax12v,mamax13v,mamax14v,mamax15v,mamax16v,mamax17v,mamax18v,
                     mamax19v,mamax20v,mamax21v,mamax22v,mamax23v,ma24v,ma25v,ma26v,ma27v,
                     ma28v,ma29v,ma30v,ma31v,ma32v,ma33v,ma34v,ma35v,
                     ma37v,ma39v,ma40v,ml13v,ml14v,
                     mh14v,mh16v,mh26v,ml17v,ml18v,
                     fl1v,fl2v,fh1v,fh2v,fh3v,
                     fh4v,dl1v,dl2v,dl4v,dl5v,
                     dl6v,dl9v,dl10v,dl18v,dh5v,
                     dh10v,tl1v,tl2v,th1v,th2v,
                     ra1v,ra3v,ra4v,l7Q10v,l7Q2v,return_10v,
                     comment)
colnames(statsout)<-c('site_no','min_date','max_date','mean_of_annual_flows','median_of_annual_flows','cv_of_annual_flows',
                      'flow_10_obs,','flow_25_obs','flow_50_obs','flow_75_obs','flow_90_obs',
                      'cv_daily_flows','ma1_mean_disc','ma2_median_disc','ma3_mean_annual_var','ma5_skew','ma12_jan_mean','ma13_feb_mean',
                      'ma14_mar_mean','ma15_apr_mean','ma16_may_mean','ma17_june_mean','ma18_july_mean','ma19_aug_mean','ma20_sep_mean',
                      'ma21_oct_mean','ma22_nov_mean','ma23_dec_mean','jan_min','feb_min','mar_min','apr_min','may_min','jun_min','jul_min',
                      'aug_min','sept_min','oct_min','nov_min','dec_min','jan_max','feb_max','mar_max','apr_max','may_max','june_max','jul_max',
                      'aug_max','sept_max','oct_max','nov_max','dec_max','ma24_jan_var','ma25_feb_var','ma26_mar_var','ma27_apr_var',
                      'ma28_may_var','ma29_jun_var','ma30_july_var','ma31_aug_var','ma32_sep_var','ma33_oct_var','ma34_nov_var','ma35_dec_var',
                      'ma37_var_across_months','ma39_monthly_std_dev','ma40_monthly_skewness','ml13_min_monthly_var','ml14_min_annual_flow',
                      'mh14_med_annual_max','mh16_high_flow_index','mh26_high_peak_flow','ml17_base_flow','ml18_base_flow_var',
                      'fl1_low_flood_pulse','fl2_low_pulse_var','fh1_high_pulse_count','fh2_high_pulse_var','fh3_high_pulse_count_three',
                      'fh4_high_pulse_count_seven','dl1_min_daily_flow','dl2_min_3_day_avg','dl4_min_30_day_avg','dl5_min_90_day_avg',
                      'dl6_min_flow_var','dl9_min_30_day_var','dl10_min_90_day_var','dl18_zero_flow_days','dh5_max_90_day_avg',
                      'dh10_max_90_day_var','tl1_min_flow_julian_day','tl2_min_julian_var','th1_max_flow_julian_day','th2_max_julian_var',
                      'ra1_rise_rate','ra3_fall_rate','ra4_fall_rate_var','7Q10_obs','7Q2_obs','10_year_return_max_obs',
                      'comment')
output="output.txt"
write.table(statsout,file="output.txt",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

# wps.out: output, text, output_file, A file containing the mean daily flow median daily flow and skewness of daily flow;
