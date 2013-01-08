# wps.des: id=test_stats, title = test stats, abstract = Finds the mean daily flow median daily flow and skewness of daily flow in the input dataset;
# wps.in: model_url, string, SOS Endpoint, A fully formed SOS GetObservations request that will return a SWE common CSV block holding date and flow;


library(XML)
library(zoo)
library(chron)
library(doBy)
library(hydroGOF)
library(HITHATStats)

sos_url_temp="http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering_temp='00003'
property_temp='00060'
drainage_url="http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="

scenario_url=paste(substr(model_url,1,regexpr("Get",model_url)-1),"GetCapabilities&service=SOS&version=1.0.0",sep="")

SWE_CSV_IHA <- function(input) {
  cat(paste("Retrieving data from: \n", input, "\n", 
            sep = " "))
  content<-paste(readLines(input,warn=FALSE))
  if (length(sapply(content,nchar))>1) { 
    flow <- read.delim(header = F, comment.char = "", 
                       as.is = T, sep = ",", text = xpathApply(xmlParse(input), 
                                                               "//swe:values", xmlValue)[[1]])
    nms <- c("date", "discharge")
    names(flow) <- nms
    flow$date <- as.POSIXct(strptime(flow$date, format = "%Y-%m-%dT%H:%M:%SZ"))
    flow$discharge <- as.numeric(flow$discharge)
    flow <- as.data.frame(flow)
    attr(flow, "SRC") <- input
    class(flow) <- c("flow", "data.frame")
    cat("Finished!\n")
    return(flow)
  } else {
    cat("No data available for site\n")
    flow<-""
    return(flow)}
}

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

getScenarioSites <- function(scenario_url){
  cat(paste("Retrieving list of sites in scenario from: \n", scenario_url, "\n", sep=" "))
  doc<-xmlTreeParse(scenario_url, getDTD=F, useInternalNodes=TRUE)
  sites<-xpathSApply(doc, "//@gml:id")
  scenario_sites<-vector(length=length(sites))
  scenario_sites<-unname(sites)
  modprop<-xpathSApply(doc, "//*[local-name() = 'observedProperty']/@xlink:href")[["href"]]
  getcap<-list(scenario_sites=scenario_sites,modprop=modprop)
  return (getcap)
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
cv <- function(x) {
  x1 <- ma1(x)
  x2 <- sdev(x)
  skew <- x2/x1
  return(skew)
}
sdev <- function(x) {
  sdev <- sd(x$discharge,na.rm=TRUE)
  return(sdev)
}

monthly.mean.ts <- function(qfiletempf,modsite) {
  meanmonts <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanmonts) <- c("Year","Month","Mean_disch")
  return(meanmonts)
}

system("del graph*png")
system("del monthly*txt")
#setwd('/Users/jlthomps/Documents/R/')
#a<-read.csv(header=F,colClasses=c("character"),text=sites)
#a2<-read.csv(header=F,colClasses=c("character"),text=sites)
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
modsites<-a2
sites<-a
yv<-vector(length=al)
ymaxv<-vector(length=al)
ma1v2<-vector(length=al)
ma2v2<-vector(length=al)
ma3v2<-vector(length=al)
ma4v2<-vector(length=al)
ma5v2<-vector(length=al)
ma6v2<-vector(length=al)
ma7v2<-vector(length=al)
ma8v2<-vector(length=al)
ma9v2<-vector(length=al)
ma10v2<-vector(length=al)
ma11v2<-vector(length=al)
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
ma24v2<-vector(length=al)
ma25v2<-vector(length=al)
ma26v2<-vector(length=al)
ma27v2<-vector(length=al)
ma28v2<-vector(length=al)
ma29v2<-vector(length=al)
ma30v2<-vector(length=al)
ma31v2<-vector(length=al)
ma32v2<-vector(length=al)
ma33v2<-vector(length=al)
ma34v2<-vector(length=al)
ma35v2<-vector(length=al)
ma36v2<-vector(length=al)
ma37v2<-vector(length=al)
ma38v2<-vector(length=al)
ma39v2<-vector(length=al)
ma40v2<-vector(length=al)
ma41v2<-vector(length=al)
ma42v2<-vector(length=al)
ma43v2<-vector(length=al)
ma44v2<-vector(length=al)
ma45v2<-vector(length=al)
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
ml13v2<-vector(length=al)
ml14v2<-vector(length=al)
ml15v2<-vector(length=al)
ml16v2<-vector(length=al)
ml17v2<-vector(length=al)
ml18v2<-vector(length=al)
ml19v2<-vector(length=al)
ml20v2<-vector(length=al)
ml21v2<-vector(length=al)
ml22v2<-vector(length=al)
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
mh13v2<-vector(length=al)
mh14v2<-vector(length=al)
mh15v2<-vector(length=al)
mh16v2<-vector(length=al)
mh17v2<-vector(length=al)
mh18v2<-vector(length=al)
mh19v2<-vector(length=al)
mh20v2<-vector(length=al)
mh21v2<-vector(length=al)
mh22v2<-vector(length=al)
mh23v2<-vector(length=al)
mh24v2<-vector(length=al)
mh25v2<-vector(length=al)
mh26v2<-vector(length=al)
mh27v2<-vector(length=al)
fl1v2<-vector(length=al)
fl2v2<-vector(length=al)
fh1v2<-vector(length=al)
fh2v2<-vector(length=al)
fh3v2<-vector(length=al)
fh4v2<-vector(length=al)
dl1v2<-vector(length=al)
dl2v2<-vector(length=al)
dl4v2<-vector(length=al)
dl5v2<-vector(length=al)
dl6v2<-vector(length=al)
dl9v2<-vector(length=al)
dl10v2<-vector(length=al)
dl18v2<-vector(length=al)
dh5v2<-vector(length=al)
dh10v2<-vector(length=al)
tl1v2<-vector(length=al)
tl2v2<-vector(length=al)
th1v2<-vector(length=al)
th2v2<-vector(length=al)
ra1v2<-vector(length=al)
ra3v2<-vector(length=al)
ra4v2<-vector(length=al)
l7Q10v2<-vector(length=al)
l7Q2v2<-vector(length=al)
return_10v2<-vector(length=al)

ma1diff<-vector(length=al)
ma2diff<-vector(length=al)
ma3diff<-vector(length=al)
ma4diff<-vector(length=al)
ma5diff<-vector(length=al)
ma5diff<-vector(length=al)
ma6diff<-vector(length=al)
ma7diff<-vector(length=al)
ma8diff<-vector(length=al)
ma9diff<-vector(length=al)
ma10diff<-vector(length=al)
ma11diff<-vector(length=al)
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
ma24diff<-vector(length=al)
ma25diff<-vector(length=al)
ma26diff<-vector(length=al)
ma27diff<-vector(length=al)
ma28diff<-vector(length=al)
ma29diff<-vector(length=al)
ma30diff<-vector(length=al)
ma31diff<-vector(length=al)
ma32diff<-vector(length=al)
ma33diff<-vector(length=al)
ma34diff<-vector(length=al)
ma35diff<-vector(length=al)
ma36diff<-vector(length=al)
ma37diff<-vector(length=al)
ma38diff<-vector(length=al)
ma39diff<-vector(length=al)
ma40diff<-vector(length=al)
ma41diff<-vector(length=al)
ma42diff<-vector(length=al)
ma43diff<-vector(length=al)
ma44diff<-vector(length=al)
ma45diff<-vector(length=al)
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
ml13diff<-vector(length=al)
ml14diff<-vector(length=al)
ml15diff<-vector(length=al)
ml16diff<-vector(length=al)
ml17diff<-vector(length=al)
ml18diff<-vector(length=al)
ml19diff<-vector(length=al)
ml20diff<-vector(length=al)
ml21diff<-vector(length=al)
ml22diff<-vector(length=al)
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
mh13diff<-vector(length=al)
mh14diff<-vector(length=al)
mh15diff<-vector(length=al)
mh16diff<-vector(length=al)
mh17diff<-vector(length=al)
mh18diff<-vector(length=al)
mh19diff<-vector(length=al)
mh20diff<-vector(length=al)
mh21diff<-vector(length=al)
mh22diff<-vector(length=al)
mh23diff<-vector(length=al)
mh24diff<-vector(length=al)
mh25diff<-vector(length=al)
mh26diff<-vector(length=al)
mh27diff<-vector(length=al)
fl1diff<-vector(length=al)
fl2diff<-vector(length=al)
fh1diff<-vector(length=al)
fh2diff<-vector(length=al)
fh3diff<-vector(length=al)
fh4diff<-vector(length=al)
dl1diff<-vector(length=al)
dl2diff<-vector(length=al)
dl4diff<-vector(length=al)
dl5diff<-vector(length=al)
dl6diff<-vector(length=al)
dl9diff<-vector(length=al)
dl10diff<-vector(length=al)
dl18diff<-vector(length=al)
dh5diff<-vector(length=al)
dh10diff<-vector(length=al)
tl1diff<-vector(length=al)
tl2diff<-vector(length=al)
th1diff<-vector(length=al)
th2diff<-vector(length=al)
ra1diff<-vector(length=al)
ra3diff<-vector(length=al)
ra4diff<-vector(length=al)
l7Q10diff<-vector(length=al)
l7Q2diff<-vector(length=al)
return_10diff<-vector(length=al)
comment<-vector(length=al)
mean_flow<-vector(length=al)
med_flow<-vector(length=al)
cv_flow<-vector(length=al)
cv_daily<-vector(length=al)
mean_flow_mod<-vector(length=al)
med_flow_mod<-vector(length=al)
cv_flow_mod<-vector(length=al)
cv_daily_mod<-vector(length=al)
mean_flow_diff<-vector(length=al)
med_flow_diff<-vector(length=al)
cv_flow_diff<-vector(length=al)
cv_daily_diff<-vector(length=al)
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
dfcvbyyrf_list<-vector(mode="list")


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
      x<-(x_mod$date)
      x_mod<-data.frame(strptime(x, "%Y-%m-%d"),x_mod$discharge)
      colnames(x_mod)<-c("date","discharge")
      x2<-(x_obs$date)
      x_obs<-data.frame(strptime(x2, "%Y-%m-%d"),x_obs$discharge)
      colnames(x_obs)<-c("date","discharge")

      x_mod<-x_mod[x_mod$date>=min(x_obs$date) & x_mod$date<=max(x_obs$date), ]
      drain_url<-paste(drainage_url,sites,sep="")
      drain_area<-getDrainageArea(drain_url)
      
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
      #flowdata<-data.frame(qfiletempf$date,qfiletempf$discharge,qfiletempf$month_val,qfiletempf$year_val,qfiletempf$day_val,qfiletempf$jul_val)  
      #colnames(flowdata)<-c('date','discharge','month_val','year_val','day_val','jul_val')
      selqfile2<-x_mod
      tempdatafr2<-NULL
      tempdatafr2<-data.frame(selqfile2)
      month_val<-rep(0,length(tempdatafr2$date))
      year_val<-rep(0,length(tempdatafr2$date))
      day_val<-rep(0,length(tempdatafr2$date))
      jul_val<-rep(0,length(tempdatafr2$date))
      wy_val<-rep(0,length(tempdatafr2$date))
      ones_val<-rep(1,length(tempdatafr2$date))
      qfiletempf2<-data.frame(tempdatafr2$date,tempdatafr2$discharge,month_val,year_val,day_val,jul_val,wy_val)
      colnames(qfiletempf2)<-c('date','discharge','month_val','year_val','day_val','jul_val','wy_val')
      qfiletempf2$month_val<-substr(x_mod$date,6,7)
      as.numeric(qfiletempf2$month_val)
      qfiletempf2$year_val<-substr(x_mod$date,3,4)
      as.numeric(qfiletempf2$year_val)
      qfiletempf2$day_val<-substr(x_mod$date,9,10)
      as.numeric(qfiletempf2$day_val)
      qfiletempf2$jul_val<-strptime(x_mod$date, "%Y-%m-%d")$yday+1
      as.numeric(qfiletempf2$jul_val)
      qfiletempf2$wy_val<-ifelse(as.numeric(qfiletempf2$month_val)>=10,as.character(as.numeric(qfiletempf2$year_val)+ones_val),qfiletempf2$year_val) 
      
      countbyyr<-aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), length)
      countbyyr_mod<-aggregate(qfiletempf2$discharge, list(qfiletempf2$wy_val), length)
      colnames(countbyyr)<-c('wy','num_samples')
      colnames(countbyyr_mod)<-c('wy','num_samples')
      sub_countbyyr<-subset(countbyyr,num_samples >= 365)
      sub_countbyyr_mod<-subset(countbyyr_mod,num_samples >= 365)
      include_yrs<-merge(sub_countbyyr,sub_countbyyr_mod)
      obs_data<-merge(qfiletempf,include_yrs,by.x="wy_val",by.y="wy")
      mod_data<-merge(qfiletempf2,include_yrs,by.x="wy_val",by.y="wy")
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
        monthly_mean<-monthly.mean.ts(obs_data,sites)
        write.table(monthly_mean,file=file,col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
        file<-paste("monthly_mean_ts_mod",toString(sites),".txt",sep="")
        monthly_mean<-monthly.mean.ts(mod_data,sites)
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
        sdbyyr_mod <- aggregate(mod_data$discharge, list(mod_data$year_val), 
                                FUN = sd, na.rm=TRUE)
        colnames(sdbyyr_mod) <- c("Year", "sdq")
        meanbyyr_mod <- aggregate(mod_data$discharge, list(mod_data$year_val), 
                                  mean, na.rm=TRUE)
        colnames(meanbyyr_mod) <- c("Year", "meanq")
        medbyyr_mod <- aggregate(mod_data$discharge, list(mod_data$year_val), 
                                 median, na.rm=TRUE)
        colnames(medbyyr_mod) <- c("Year","medq")
        dfcvbyyr_mod <- data.frame(meanbyyr_mod$Year, sdbyyr_mod$sdq, 
                                   meanbyyr_mod$meanq, medbyyr_mod$medq)
        colnames(dfcvbyyr_mod) <- c("Year", "sdq", "meanq", "medq")
        cvbyyr_mod <- dfcvbyyr_mod$sdq/dfcvbyyr_mod$meanq
        dfcvbyyrf_mod <- data.frame(dfcvbyyr_mod, cvbyyr_mod)
        colnames(dfcvbyyrf_mod) <- c("Year", "sdq", "meanq", "medq", 
                                     "cvq")
        
        mean_flow[i]<-mean(dfcvbyyrf$meanq,na.rm=TRUE)
        med_flow[i]<-median(dfcvbyyrf$meanq,na.rm=TRUE)
        cv_flow[i]<-sd(dfcvbyyrf$meanq,na.rm=TRUE)/mean(dfcvbyyrf$meanq,na.rm=TRUE)
        cv_daily[i]<-cv(obs_data)
        mean_flow_mod[i]<-mean(dfcvbyyrf_mod$meanq,na.rm=TRUE)
        med_flow_mod[i]<-median(dfcvbyyrf_mod$meanq,na.rm=TRUE)
        cv_flow_mod[i]<-sd(dfcvbyyrf_mod$meanq,na.rm=TRUE)/mean(dfcvbyyrf_mod$meanq,na.rm=TRUE)
        cv_daily_mod[i]<-cv(mod_data)
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
        
        ma1v2[i]<-ma1(mod_data)
        ma2v2[i]<-ma2(mod_data)
        ma3v2[i]<-ma3(mod_data)
        ma4v2[i]<-unlist(ma4.11(mod_data)[1])
        ma5v2[i]<-unlist(ma4.11(mod_data)[2])
        ma6v2[i]<-unlist(ma4.11(mod_data)[3])
        ma7v2[i]<-unlist(ma4.11(mod_data)[4])
        ma8v2[i]<-unlist(ma4.11(mod_data)[5])
        ma9v2[i]<-unlist(ma4.11(mod_data)[6])
        ma10v2[i]<-unlist(ma4.11(mod_data)[7])
        ma11v2[i]<-unlist(ma4.11(mod_data)[8])
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
        ma24v2[i]<-ma24.35(mod_data)[1,1]
        ma25v2[i]<-ma24.35(mod_data)[2,1]
        ma26v2[i]<-ma24.35(mod_data)[3,1]
        ma27v2[i]<-ma24.35(mod_data)[4,1]
        ma28v2[i]<-ma24.35(mod_data)[5,1]
        ma29v2[i]<-ma24.35(mod_data)[6,1]
        ma30v2[i]<-ma24.35(mod_data)[7,1]
        ma31v2[i]<-ma24.35(mod_data)[8,1]
        ma32v2[i]<-ma24.35(mod_data)[9,1]
        ma33v2[i]<-ma24.35(mod_data)[10,1]
        ma34v2[i]<-ma24.35(mod_data)[11,1]
        ma35v2[i]<-ma24.35(mod_data)[12,1]
        ma36v2[i]<-unlist(ma36.40(mod_data)[1])
        ma37v2[i]<-unlist(ma36.40(mod_data)[2])
        ma38v2[i]<-unlist(ma36.40(mod_data)[3])
        ma39v2[i]<-unlist(ma36.40(mod_data)[4])
        ma40v2[i]<-unlist(ma36.40(mod_data)[5])
        ma41v2[i]<-unlist(ma41.45(mod_data,drain_area)[1])
        ma42v2[i]<-unlist(ma41.45(mod_data,drain_area)[2])
        ma43v2[i]<-unlist(ma41.45(mod_data,drain_area)[3])
        ma44v2[i]<-unlist(ma41.45(mod_data,drain_area)[4])
        ma45v2[i]<-unlist(ma41.45(mod_data,drain_area)[5])
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
        ml13v2[i]<-ml13(mod_data)
        ml14v2[i]<-unlist(ml14.16(mod_data)[1])
        ml15v2[i]<-unlist(ml14.16(mod_data)[2])
        ml16v2[i]<-unlist(ml14.16(mod_data)[3])
        ml17v2[i]<-ml17(mod_data)
        ml18v2[i]<-ml18(mod_data)
        ml19v2[i]<-ml19(mod_data)
        ml20v2[i]<-ml20(mod_data)
        ml21v2[i]<-ml21(mod_data)
        ml22v2[i]<-ml22(mod_data,drain_area)
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
        mh13v2[i]<-mh13(mod_data)
        mh14v2[i]<-mh14(mod_data)
        mh15v2[i]<-unlist(mh15.17(mod_data)[1])
        mh16v2[i]<-unlist(mh15.17(mod_data)[2])
        mh17v2[i]<-unlist(mh15.17(mod_data)[3])
        mh18v2[i]<-mh18(mod_data)
        mh19v2[i]<-mh19(mod_data)
        mh20v2[i]<-mh20(mod_data,drain_area)
        mh21v2[i]<-mh21(mod_data)
        mh22v2[i]<-mh22(mod_data)
        mh23v2[i]<-mh23(mod_data)
        mh24v2[i]<-mh24(mod_data)
        mh25v2[i]<-mh25(mod_data)
        mh26v2[i]<-mh26(mod_data)
        mh27v2[i]<-mh27(mod_data)
        fl1v2[i]<-fl1(mod_data)
        fl2v2[i]<-fl2(mod_data)
        fh1v2[i]<-fh1(mod_data)
        fh2v2[i]<-fh2(mod_data)
        fh3v2[i]<-fh3(mod_data)
        fh4v2[i]<-fh4(mod_data)
        dl1v2[i]<-dl1(mod_data)
        dl2v2[i]<-dl2(mod_data)
        dl4v2[i]<-dl4(mod_data)
        dl5v2[i]<-dl5(mod_data)
        dl6v2[i]<-dl6(mod_data)
        dl9v2[i]<-dl9(mod_data)
        dl10v2[i]<-dl10(mod_data)
        dl18v2[i]<-dl18(mod_data)
        dh5v2[i]<-dh5(mod_data)
        dh10v2[i]<-dh10(mod_data)
        tl1v2[i]<-tl1(mod_data)
        tl2v2[i]<-tl2(mod_data)
        th1v2[i]<-th1(mod_data)
        th2v2[i]<-th2(mod_data)
        ra1v2[i]<-ra1(mod_data)
        ra3v2[i]<-ra3(mod_data)
        ra4v2[i]<-ra4(mod_data)
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

ma1vdiff<-abs(ma1v-ma1v2)
ma2vdiff<-abs(ma2v-ma2v2)
ma3vdiff<-abs(ma3v-ma3v2)
ma4vdiff<-abs(ma4v-ma4v2)
ma5vdiff<-abs(ma5v-ma5v2)
ma6vdiff<-abs(ma6v-ma6v2)
ma7vdiff<-abs(ma7v-ma7v2)
ma8vdiff<-abs(ma8v-ma8v2)
ma9vdiff<-abs(ma9v-ma9v2)
ma10vdiff<-abs(ma10v-ma10v2)
ma11vdiff<-abs(ma11v-ma11v2)
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
ma24vdiff<-abs(ma24v-ma24v2)
ma25vdiff<-abs(ma25v-ma25v2)
ma26vdiff<-abs(ma26v-ma26v2)
ma27vdiff<-abs(ma27v-ma27v2)
ma28vdiff<-abs(ma28v-ma28v2)
ma29vdiff<-abs(ma29v-ma29v2)
ma30vdiff<-abs(ma30v-ma30v2)
ma31vdiff<-abs(ma31v-ma31v2)
ma32vdiff<-abs(ma32v-ma32v2)
ma33vdiff<-abs(ma33v-ma33v2)
ma34vdiff<-abs(ma34v-ma34v2)
ma35vdiff<-abs(ma35v-ma35v2)
ma36vdiff<-abs(ma36v-ma36v2)
ma37vdiff<-abs(ma37v-ma37v2)
ma38vdiff<-abs(ma38v-ma38v2)
ma39vdiff<-abs(ma39v-ma39v2)
ma40vdiff<-abs(ma40v-ma40v2)
ma41vdiff<-abs(ma41v-ma41v2)
ma42vdiff<-abs(ma42v-ma42v2)
ma43vdiff<-abs(ma43v-ma43v2)
ma44vdiff<-abs(ma44v-ma44v2)
ma45vdiff<-abs(ma45v-ma45v2)
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
ml13vdiff<-abs(ml13v-ml13v2)
ml14vdiff<-abs(ml14v-ml14v2)
ml15vdiff<-abs(ml15v-ml15v2)
ml16vdiff<-abs(ml16v-ml16v2)
ml17vdiff<-abs(ml17v-ml17v2)
ml18vdiff<-abs(ml18v-ml18v2)
ml19vdiff<-abs(ml19v-ml19v2)
ml20vdiff<-abs(ml20v-ml20v2)
ml21vdiff<-abs(ml21v-ml21v2)
ml22vdiff<-abs(ml22v-ml22v2)
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
mh13vdiff<-abs(mh13v-mh13v2)
mh14vdiff<-abs(mh14v-mh14v2)
mh15vdiff<-abs(mh15v-mh15v2)
mh16vdiff<-abs(mh16v-mh16v2)
mh17vdiff<-abs(mh17v-mh17v2)
mh18vdiff<-abs(mh18v-mh18v2)
mh19vdiff<-abs(mh19v-mh19v2)
mh20vdiff<-abs(mh20v-mh20v2)
mh21vdiff<-abs(mh21v-mh21v2)
mh22vdiff<-abs(mh22v-mh22v2)
mh23vdiff<-abs(mh23v-mh23v2)
mh24vdiff<-abs(mh24v-mh24v2)
mh25vdiff<-abs(mh25v-mh25v2)
mh26vdiff<-abs(mh26v-mh26v2)
mh27vdiff<-abs(mh27v-mh27v2)
fl1vdiff<-abs(fl1v-fl1v2)
fl2vdiff<-abs(fl2v-fl2v2)
fh1vdiff<-abs(fh1v-fh1v2)
fh2vdiff<-abs(fh2v-fh2v2)
fh3vdiff<-abs(fh3v-fh3v2)
fh4vdiff<-abs(fh4v-fh4v2)
dl1vdiff<-abs(dl1v-dl1v2)
dl2vdiff<-abs(dl2v-dl2v2)
dl4vdiff<-abs(dl4v-dl4v2)
dl5vdiff<-abs(dl5v-dl5v2)
dl6vdiff<-abs(dl6v-dl6v2)
dl9vdiff<-abs(dl9v-dl9v2)
dl10vdiff<-abs(dl10v-dl10v2)
dl18vdiff<-abs(dl18v-dl18v2)
dh5vdiff<-abs(dh5v-dh5v2)
dh10vdiff<-abs(dh10v-dh10v2)
tl1vdiff<-abs(tl1v-tl1v2)
tl2vdiff<-abs(tl2v-tl2v2)
th1vdiff<-abs(th1v-th1v2)
th2vdiff<-abs(th2v-th2v2)
ra1vdiff<-abs(ra1v-ra1v2)
ra3vdiff<-abs(ra3v-ra3v2)
ra4vdiff<-abs(ra4v-ra4v2)
l7Q10diff<-abs(l7Q10v-l7Q10v2)
l7Q2diff<-abs(l7Q2v-l7Q2v2)
return_10diff<-abs(return_10v-return_10v2)
mean_flow_diff<-abs(mean_flow-mean_flow_mod)
med_flow_diff<-abs(med_flow-med_flow_mod)
cv_flow_diff<-abs(cv_flow-cv_flow_mod)
cv_daily_diff<-abs(cv_daily-cv_daily_mod)
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
                     cv_daily,ma1v,ma2v,ma3v,ma4v,ma5v,ma6v,ma7v,ma8v,ma9v,ma10v,ma11v,ma12v,ma13v,
                     ma14v,ma15v,ma16v,ma17v,ma18v,ma19v,ma20v,
                     ma21v,ma22v,ma23v,ma24v,ma25v,ma26v,ma27v,
                     ma28v,ma29v,ma30v,ma31v,ma32v,ma33v,ma34v,ma35v,ma36v,
                     ma37v,ma38v,ma39v,ma40v,ma41v,ma42v,ma43v,ma44v,ma45v,ml1v,ml2v,ml3v,ml4v,ml5v,ml6v,ml7v,ml8v,ml9v,
                     ml10v,ml11v,ml12v,ml13v,ml14v,ml15v,ml16v,ml17v,ml18v,ml19v,ml20v,ml21v,ml22v,mh1v,mh2v,mh3v,mh4v,mh5v,
                     mh6v,mh7v,mh8v,mh9v,mh10v,mh11v,mh12v,mh13v,mh14v,mh15v,mh16v,mh17v,mh18v,mh19v,mh20v,mh21v,
                     mh22v,mh23v,mh24v,mh25v,mh26v,mh27v,
                     fl1v,fl2v,fh1v,fh2v,fh3v,
                     fh4v,dl1v,dl2v,dl4v,dl5v,
                     dl6v,dl9v,dl10v,dl18v,dh5v,
                     dh10v,tl1v,tl2v,th1v,th2v,
                     ra1v,ra3v,ra4v,l7Q10v,l7Q2v,return_10v,
                     mean_flow_mod,med_flow_mod,cv_flow_mod,cv_daily_mod,
                     flow_10_mod,flow_25_mod,flow_50_mod,flow_75_mod,flow_90_mod,
                     ma1v2,ma2v2,ma3v2,ma4v2,ma5v2,ma6v2,ma7v2,ma8v2,ma9v2,ma10v2,ma11v2,ma12v2,ma13v2,
                     ma14v2,ma15v2,ma16v2,ma17v2,ma18v2,ma19v2,ma20v2,
                     ma21v2,ma22v2,ma23v2,ma24v2,ma25v2,ma26v2,ma27v2,
                     ma28v2,ma29v2,ma30v2,ma31v2,ma32v2,ma33v2,ma34v2,ma35v2,ma36v2,
                     ma37v2,ma38v2,ma39v2,ma40v2,ma41v2,ma42v2,ma43v2,ma44v2,ma45v2,ml1v2,ml2v2,ml3v2,ml4v2,ml5v2,ml6v2,ml7v2,ml8v2,ml9v2,
                     ml10v2,ml11v2,ml12v2,ml13v2,ml14v2,ml15v2,ml16v2,ml17v2,ml18v2,ml19v2,ml20v2,ml21v2,ml22v2,mh1v2,mh2v2,mh3v2,mh4v2,mh5v2,
                     mh6v2,mh7v2,mh8v2,mh9v2,mh10v2,mh11v2,mh12v2,mh13v2,mh14v2,mh15v2,mh16v2,mh17v2,mh18v2,mh19v2,mh20v2,mh21v2,
                     mh22v,mh23v,mh24v,mh25v,mh26v,mh27v2,
                     fl1v2,fl2v2,fh1v2,fh2v2,fh3v2,
                     fh4v2,dl1v2,dl2v2,dl4v2,dl5v2,
                     dl6v2,dl9v2,dl10v2,dl18v2,dh5v2,
                     dh10v2,tl1v2,tl2v2,th1v2,th2v2,
                     ra1v2,ra3v2,ra4v2,l7Q10v2,l7Q2v2,return_10v2,mean_flow_diff,med_flow_diff,cv_flow_diff,cv_daily_diff,
                     flow_10_diff,flow_25_diff,flow_50_diff,flow_75_diff,flow_90_diff,
                     ma1vdiff,ma2vdiff,ma3vdiff,ma4vdiff,ma5vdiff,ma6vdiff,ma7vdiff,ma8vdiff,ma9vdiff,ma10vdiff,ma11vdiff,ma12vdiff,ma13vdiff,
                     ma14vdiff,ma15vdiff,ma16vdiff,ma17vdiff,ma18vdiff,ma19vdiff,ma20vdiff,
                     ma21vdiff,ma22vdiff,ma23vdiff,ma24vdiff,ma25vdiff,ma26vdiff,ma27vdiff,
                     ma28vdiff,ma29vdiff,ma30vdiff,ma31vdiff,ma32vdiff,ma33vdiff,ma34vdiff,ma35vdiff,ma36vdiff,
                     ma37vdiff,ma38vdiff,ma39vdiff,ma40vdiff,ma41vdiff,ma42vdiff,ma43vdiff,ma44vdiff,ma45vdiff,ml1vdiff,ml2vdiff,ml3vdiff,ml4vdiff,ml5vdiff,ml6vdiff,ml7vdiff,ml8vdiff,ml9vdiff,
                     ml10vdiff,ml11vdiff,ml12vdiff,ml13vdiff,ml14vdiff,ml15vdiff,ml16vdiff,ml17vdiff,ml18vdiff,ml19vdiff,ml20vdiff,ml21vdiff,ml22vdiff,mh1vdiff,mh2vdiff,mh3vdiff,mh4vdiff,mh5vdiff,
                     mh6vdiff,mh7vdiff,mh8vdiff,mh9vdiff,mh10vdiff,mh11vdiff,mh12vdiff,mh13vdiff,mh14vdiff,mh15vdiff,mh16vdiff,mh17vdiff,mh18vdiff,mh19vdiff,mh20vdiff,mh21vdiff,
                     mh22v,mh23v,mh24v,mh25v,mh26v,mh27vdiff,
                     fl1vdiff,fl2vdiff,fh1vdiff,fh2vdiff,fh3vdiff,
                     fh4vdiff,dl1vdiff,dl2vdiff,dl4vdiff,dl5vdiff,
                     dl6vdiff,dl9vdiff,dl10vdiff,dl18vdiff,dh5vdiff,
                     dh10vdiff,tl1vdiff,tl2vdiff,th1vdiff,th2vdiff,
                     ra1vdiff,ra3vdiff,ra4vdiff,l7Q10diff,l7Q2diff,return_10diff,pbiasv,comment)
colnames(statsout)<-c('site_no','nse','nselog','rmse','min_date','max_date','mean_of_annual_flows','median_of_annual_flows','cv_of_annual_flows',
                      'nse_90','nse_75_90','nse_50_75','nse_25_50','nse_10_25','nse_10',
                      'rmse_90','rmse_75_90','rmse_50_75','rmse_25_50','rmse_10_25','rmse_10',
                      'pbias_90','pbias_75_90','pbias_50_75','pbias_25_50','pbias_10_25','pbias_10',
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
                      'fh4_high_pulse_count_seven','dl1_min_daily_flow','dl2_min_3_day_avg','dl4_min_30_day_avg','dl5_min_90_day_avg',
                      'dl6_min_flow_var','dl9_min_30_day_var','dl10_min_90_day_var','dl18_zero_flow_days','dh5_max_90_day_avg',
                      'dh10_max_90_day_var','tl1_min_flow_julian_day','tl2_min_julian_var','th1_max_flow_julian_day','th2_max_julian_var',
                      'ra1_rise_rate','ra3_fall_rate','ra4_fall_rate_var','7Q10_obs','7Q2_obs','10_year_return_max_obs',
                      'mean_of_annual_flows_mod','median_of_annual_flows_mod','cv_of_annual_flows_mod','cv_daily_flows_mod',
                      'flow_10_mod_mod','flow_25_mod_mod','flow_50_mod_mod','flow_75_mod_mod','flow_90_mod_mod','ma1_mean_disc_mod','ma2_median_disc_mod','ma3_mean_annual_var_mod','ma4_mod','ma5_skew_mod','ma6_mod','ma7_mod','ma8_mod','ma9_mod','ma10_mod','ma11_mod','ma12_jan_mean_mod','ma13_feb_mean_mod',
                      'ma14_mar_mean_mod','ma15_apr_mean_mod','ma16_may_mean_mod','ma17_june_mean_mod','ma18_july_mean_mod','ma19_aug_mean_mod','ma20_sep_mean_mod',
                      'ma21_oct_mean_mod','ma22_nov_mean_mod','ma23_dec_mean_mod','ma24_jan_var_mod','ma25_feb_var_mod','ma26_mar_var_mod','ma27_apr_var_mod',
                      'ma28_may_var_mod','ma29_jun_var_mod','ma30_july_var_mod','ma31_aug_var_mod','ma32_sep_var_mod','ma33_oct_var_mod','ma34_nov_var_mod','ma35_dec_var_mod','ma36_mod',
                      'ma37_var_across_months_mod','ma38_mod','ma39_monthly_std_dev_mod','ma40_monthly_skewness_mod','ma41_mod','ma42_mod','ma43_mod','ma44_mod','ma45_mod','ml1_mod','ml2_mod','ml3_mod','ml4_mod','ml5_mod','ml6_mod','ml7_mod','ml8_mod','ml9_mod',
                      'ml10_mod','ml11_mod','ml12_mod','ml13_min_monthly_var_mod','ml14_min_annual_flow_mod','ml15_mod','ml16_mod','ml17_mod','ml18_mod','ml19_mod','ml20_mod','ml21_mod','ml22_mod',
                      'mh1_mod','mh2_mod','mh3_mod','mh4_mod','mh5_mod','mh6_mod','mh7_mod','mh8_mod','mh9_mod','mh10_mod','mh11_mod','mh12_mod','mh13_mod','mh14_med_annual_max_mod',
                      'mh15_mod','mh16_high_flow_index_mod','mh17_mod','mh18_mod','mh19_mod','mh20_mod','mh21_mod','mh22_mod','mh23_mod','mh24_mod','mh25_mod','mh26_high_peak_flow_mod','mh27_mod',
                      'fl1_low_flood_pulse_mod','fl2_low_pulse_var_mod','fh1_high_pulse_count_mod','fh2_high_pulse_var_mod','fh3_high_pulse_count_three_mod',
                      'fh4_high_pulse_count_seven_mod','dl1_min_daily_flow_mod','dl2_min_3_day_avg_mod','dl4_min_30_day_avg_mod','dl5_min_90_day_avg_mod',
                      'dl6_min_flow_var_mod','dl9_min_30_day_var_mod','dl10_min_90_day_var_mod','dl18_zero_flow_days_mod','dh5_max_90_day_avg_mod',
                      'dh10_max_90_day_var_mod','tl1_min_flow_julian_day_mod','tl2_min_julian_var_mod','th1_max_flow_julian_day_mod','th2_max_julian_var_mod',
                      'ra1_rise_rate_mod','ra3_fall_rate_mod','ra4_fall_rate_var_mod','7Q10_mod_mod','7Q2_mod_mod','10_year_return_max_mod_mod','mean_flow_diff_mod','med_flow_diff_mod','cv_flow_diff_mod','cv_daily_diff_mod',
                      'flow_10_diff','flow_25_diff','flow_50_diff','flow_75_diff','flow_90_diff',
                      'ma1_mean_disc_diff','ma2_median_disc_diff','ma3_mean_annual_var_diff','ma4_diff','ma5_skew_diff','ma6_diff','ma7_diff','ma8_diff','ma9_diff','ma10_diff','ma11_diff','ma12_jan_mean_diff','ma13_feb_mean_diff',
                      'ma14_mar_mean_diff','ma15_apr_mean_diff','ma16_may_mean_diff','ma17_june_mean_diff','ma18_july_mean_diff','ma19_aug_mean_diff','ma20_sep_mean_diff',
                      'ma21_oct_mean_diff','ma22_nov_mean_diff','ma23_dec_mean_diff','ma24_jan_var_diff','ma25_feb_var_diff','ma26_mar_var_diff','ma27_apr_var_diff',
                      'ma28_may_var_diff','ma29_jun_var_diff','ma30_july_var_diff','ma31_aug_var_diff','ma32_sep_var_diff','ma33_oct_var_diff','ma34_nov_var_diff','ma35_dec_var_diff','ma36_diff',
                      'ma37_var_across_months_diff','ma38_diff','ma39_monthly_std_dev_diff','ma40_monthly_skewness_diff','ma41_diff','ma42_diff','ma43_diff','ma44_diff','ma45_diff','ml1_diff','ml2_diff','ml3_diff','ml4_diff','ml5_diff','ml6_diff','ml7_diff','ml8_diff','ml9_diff',
                      'ml10_diff','ml11_diff','ml12_diff','ml13_min_monthly_var_diff','ml14_min_annual_flow_diff','ml15_diff','ml16_diff','ml17_diff','ml18_diff','ml19_diff','ml20_diff','ml21_diff','ml22_diff',
                      'mh1_diff','mh2_diff','mh3_diff','mh4_diff','mh5_diff','mh6_diff','mh7_diff','mh8_diff','mh9_diff','mh10_diff','mh11_diff','mh12_diff','mh13_diff','mh14_med_annual_max_diff',
                      'mh15_diff','mh16_high_flow_index_diff','mh17_diff','mh18_diff','mh19_diff','mh20_diff','mh21_diff','mh22_diff','mh23_diff','mh24_diff','mh25_diff','mh26_high_peak_flow_diff','mh27_diff',
                      'fl1_low_flood_pulse_diff','fl2_low_pulse_var_diff','fh1_high_pulse_count_diff','fh2_high_pulse_var_diff','fh3_high_pulse_count_three_diff',
                      'fh4_high_pulse_count_seven_diff','dl1_min_daily_flow_diff','dl2_min_3_day_avg_diff','dl4_min_30_day_avg_diff','dl5_min_90_day_avg_diff',
                      'dl6_min_flow_var_diff','dl9_min_30_day_var_diff','dl10_min_90_day_var_diff','dl18_zero_flow_days_diff','dh5_max_90_day_avg_diff',
                      'dh10_max_90_day_var_diff','tl1_min_flow_julian_day_diff','tl2_min_julian_var_diff','th1_max_flow_julian_day_diff','th2_max_julian_var_diff',
                      'ra1_rise_rate_diff','ra3_fall_rate_diff','ra4_fall_rate_var_diff','7Q10_diff','7Q2_diff','10_year_return_max_diff','percent_bias','comment')
output="output.zip"
if (i==length(a2)) {
  write.table(statsout,file="output.txt",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
  system("del output.zip")
  system("zip -r output graph*png")
  system("zip -r output monthly*txt")
  system("zip -r output output*")
} else { 
  output="output.zip" 
  message<-"One or more web service calls resulted in failure. Please try again."
  write.table(message,file="output.txt",col.names=FALSE,row.names=FALSE,quote=FALSE)
}

# wps.out: output, zip, output_file, A file containing the mean daily flow median daily flow and skewness of daily flow;
