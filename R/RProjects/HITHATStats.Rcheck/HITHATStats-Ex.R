pkgname <- "HITHATStats"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('HITHATStats')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("getXMLWML1.1Data")
### * getXMLWML1.1Data

flush(stderr()); flush(stdout())

### Name: getXMLWML1.1Data
### Title: Function to return data from the NWISWeb WaterML1.1 daily values
###   service
### Aliases: getXMLWML1.1Data

### ** Examples

url<-"http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
sites<-"02177000"
startdate<-"1900-01-01"
enddate<-"2012-10-01"
offering='00003'
property='00060'
obs_url<-paste(url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,sep='')
getXMLWML1.1Data(obs_url)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
