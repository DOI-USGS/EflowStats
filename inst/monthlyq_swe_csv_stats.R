# wps.des: id=simple_monthly_stats, title = Simple Monthly Flow Statistics, abstract = Finds the mean and median annual and monthly statistics for a monthly flow record as well as the deciles;
# wps.in: model_url, string, SOS Endpoint, A fully formed SOS GetObservations request that will return a SWE common CSV block holding date and flow;

# model_url = 'ftp://ftpext.usgs.gov/pub/er/wi/middleton/dblodgett/example_monthly_swecsv.xml'

library(XML)
library(zoo)
library(chron)
library(doBy)
library(hydroGOF)
library(HITHATStats)
deciles <- function(x) {
  isolateq <- x$discharge
  sortq <- sort(isolateq)
  deciles<-matrix(nrow=9,ncol=2)
  deciles[1:9,1] <- seq(0.1,0.9,by=.1)
  deciles[1,2] <- sortq[floor(findrank(length(sortq),0.9))]
  deciles[2,2] <- sortq[floor(findrank(length(sortq),0.8))]
  deciles[3,2] <- sortq[floor(findrank(length(sortq),0.7))]
  deciles[4,2] <- sortq[floor(findrank(length(sortq),0.6))]
  deciles[5,2] <- sortq[floor(findrank(length(sortq),0.5))]
  deciles[6,2] <- sortq[floor(findrank(length(sortq),0.4))]
  deciles[7,2] <- sortq[floor(findrank(length(sortq),0.3))]
  deciles[8,2] <- sortq[floor(findrank(length(sortq),0.2))]
  deciles[9,2] <- sortq[floor(findrank(length(sortq),0.1))]
  return(deciles)
}
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
    flow$date <- as.Date(strptime(flow$date, format = "%Y-%m-%dT%H:%M:%SZ"))
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
x_obs=SWE_CSV_IHA(model_url)
selqfile<-x_obs
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
qfiletempf$month_val<-as.numeric(qfiletempf$month_val)
qfiletempf$year_val<-substr(x_obs$date,1,4)
qfiletempf$month_val<-substr(x_obs$date,6,7)
qfiletempf$month_val<-as.numeric(qfiletempf$month_val)
qfiletempf$year_val<-substr(x_obs$date,1,4)
qfiletempf$year_val<-as.numeric(qfiletempf$year_val)
qfiletempf$day_val<-substr(x_obs$date,9,10)
qfiletempf$day_val<-as.numeric(qfiletempf$day_val)
qfiletempf$jul_val<-strptime(x_obs$date, "%Y-%m-%d")$yday+1
qfiletempf$jul_val<-as.numeric(qfiletempf$jul_val)
qfiletempf$wy_val<-ifelse(as.numeric(qfiletempf$month_val)>=10,as.character(as.numeric(qfiletempf$year_val)+ones_val),qfiletempf$year_val)
obs_data<-qfiletempf
meanflowy<-meanflowbyyear(obs_data)
medflowy<-medflowbyyear(obs_data)
meanmonthly<-ma12.23(obs_data)
medmonthly<-ma12.23(obs_data, pref='median')
decile_list <- deciles(obs_data)
colnames(meanflowy)<-c("Year","meanq")
colnames(medflowy)<-c("Year","medianq")
colnames(meanmonthly)<-c("Month","meanq")
colnames(medmonthly)<-c("Month","medianq")
colnames(decile_list)<-c("decile","q")
output='outfile.txt'
write('mean_annual_flow', output)
write.table(meanflowy, output,sep=',', append=TRUE, row.names=FALSE)
write('median_annual_flow', output,  append=TRUE)
write.table(medflowy, output,sep=',', append=TRUE, row.names=FALSE)
write('mean_monthly_flow', output,  append=TRUE)
write.table(meanmonthly, output,sep=',', append=TRUE, row.names=FALSE)
write('median_monthly_flow', output,  append=TRUE)
write.table(medmonthly, output,sep=',', append=TRUE, row.names=FALSE)
write('deciles', output,sep=',', append=TRUE)
write.table(decile_list, output,sep=',', append=TRUE, row.names=FALSE)

# wps.out: output, text, output_file, A file containing the calculated statistics;