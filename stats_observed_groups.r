# wps.des: id=test_stats, title = test stats, abstract = Finds the mean daily flow median daily flow and skewness of daily flow in the input dataset;
# wps.in: model_url, string, SOS Endpoint, A fully formed SOS GetObservations request that will return a SWE common CSV block holding date and flow;
# wps.in: stats, string, list, a list of the requested statistic groups to return

library(XML)
library(zoo)
library(chron)
library(doBy)
library(hydroGOF)
library(RCurl)
library(lmomco)
library(HITHATStats)
library(NWCCompare)

# WPS Inputs
sites <- c("02177000", "02178400")
startdate <- "2008-10-01"
enddate <- "2013-9-30"
stats="GOF,GOFMonth,magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"

# Hardcode NWIS urls and parameters.
nwisDvUrl = "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering = "00003"
property = "00060"
drainage_url = "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
interval<-''
latest<-''

Flownum <- (length(grep("magStat",stats))*5)+(length(grep("flowStat",stats))*3)+(length(grep("durStat",stats))*3)+(length(grep("timStat",stats))*3)+(length(grep("rateStat",stats))*3)+(length(grep("otherStat",stats))*12)
Magnifnum <- (length(grep("magnifSeven",stats))*7)
comment<-vector(length=length(sites))
ObsFlowStats <- matrix(nrow=length(sites),ncol=Flownum)
magnifSevenObs <- matrix(nrow=nrow(ObsFlowStats),ncol=Magnifnum)
#MonAnnGoF <- matrix(nrow=nrow(ObsFlowStats),ncol=84) # Why is this commented ou?
yv<-vector(length=length(sites))
ymaxv<-vector(length=length(sites))
namesMagnif <- c('lam1Obs','tau2Obs','tau3Obs','tau4Obs','ar1Obs','amplitudeObs','phaseObs')
namesMagStat <- c('ma26Obs','ma41Obs','ml18Obs','ml20Obs','mh10Obs')
namesFlowStat <- c('fl2Obs','fh6Obs','fh7Obs')
namesDurStat <- c('dl6Obs','dh13Obs','dh16Obs')
namesTimStat <- c('ta1Obs','tl1Obs','th1Obs')
namesRateStat <- c('ra5Obs','ra7Obs','ra8Obs')
namesOtherStat <- c('med_flowObs','cv_flowObs','cv_dailyObs','l7Q10Obs','l7Q2Obs','return_10Obs','flow_10Obs','flow_25Obs','flow_50Obs','flow_75Obs','flow_90Obs','flow_15Obs')

namesFull <- c('site_no','min_date','max_date')

for (i in 1:length(sites)) {
    site=sites[i]
    url2<-paste(sos_url_temp,site,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering_temp,'&parameterCd=',property_temp,sep='')
    x_obs <- getXMLWML1.1Data(url2)
    
    if (nrow(x_obs)>2) {
      obs_data <- get_obsdata(x_obs)
      obs_count<-nrow(obs_data)
      cat(paste("get_obsdata run on x_obs for site",site,obs_count,"\n",sep=" "))
      drain_url<-paste(drainage_url,site,sep="")
      drain_area<-getDrainageArea(drain_url)
      cat(paste("data and drainage area retrieved for site",site,drain_area,"\n",sep=" "))
            yv[i]<-as.character(min(obs_data$date))
            ymaxv[i]<-as.character(max(obs_data$date))
            cat(paste("dates calculated for site",site,"\n",sep=" "))
            
            obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
            obs_count <- nrow(obs_data)
            cat(paste("dfs created for site",site,obs_count,"\n",sep=" "))
            if (Flownum>0) {
            ObsFlowStats[i,] <- FlowStats(obs_data,drain_area,stats)
            cat(paste("Obs flow stats calculated for site",site,"\n",sep=" "))
            }
            if (Magnifnum>0) {
            magnifSevenObs[i,] <- magnifSeven(obs_data)
            cat(paste("Obs mag7 stats calculated for site",site,"\n",sep=" "))
            }
            comment <- ""
    } else {
      comment[i]<-"No observed data for this site"
    }
}

statsout<-data.frame(sites,yv,ymaxv,magnifSevenObs,ObsFlowStats,comment,stringsAsFactors=FALSE)

if (Magnifnum>0) {
  namesFull <- c(namesFull,namesMagnif)
}
if (length(grep("otherStat",stats))>0) {
  namesFull <- c(namesFull,namesOtherStat)
}
if (length(grep("magStat",stats))>0) {
  namesFull <- c(namesFull,namesMagStat)
}
if (length(grep("flowStat",stats))>0) {
  namesFull <- c(namesFull,namesFlowStat)
}
if (length(grep("durStat",stats))>0) {
  namesFull <- c(namesFull,namesDurStat)
}
if (length(grep("timStat",stats))>0) {
  namesFull <- c(namesFull,namesTimStat)
}
if (length(grep("rateStat",stats))>0) {
  namesFull <- c(namesFull,namesRateStat)
}
namesFull <- c(namesFull,'comment')

colnames(statsout)<-namesFull
cat("statsout created and named \n")
output="output.zip"
if (i==length(sites)) {
  write.table(statsout,file="output.txt",col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
  system("rm output.zip")
  #system("zip -r output graph*png")
  #system("zip -r output monthly*txt")
  system("zip -r output output*")
} else { 
  output="output.zip" 
  message<-"One or more web service calls resulted in failure. Please try again."
  write.table(message,file="output.txt",col.names=FALSE,row.names=FALSE,quote=FALSE)
}

# wps.out: output, zip, output_file, A file containing the table of statistics as well as monthly stats and graphs for each site;
