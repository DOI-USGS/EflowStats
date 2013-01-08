#' Function to return data from the NWISWeb WaterML1.1 daily values service
#' 
#' This function accepts a url parameter that already contains the desired 
#' NWIS site, parameter code, statistic, startdate and enddate. It returns a 
#' data frame containing "Date" and "Discharge"
#' 
#' @param obs_url string containing the url for the retrieval
#' @return Daily a data frame containing columns 'Date' and 'Discharge'
#' @export
#' @examples
#' url<-"http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
#' sites<-"02177000"
#' startdate<-"2012-09-01"
#' enddate<-"2012-10-01"
#' offering<-'00003'
#' property<-'00060'
#' obs_url<-paste(url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,sep='')
#' getXMLWML1.1Data(obs_url)
getXMLWML1.1Data <-
function(obs_url){
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
