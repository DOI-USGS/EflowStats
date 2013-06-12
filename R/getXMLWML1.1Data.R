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
#' \dontrun{url<-"http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="}
#' \dontrun{sites<-"02177000"}
#' \dontrun{startdate<-"2012-09-01"}
#' \dontrun{enddate<-"2012-10-01"}
#' \dontrun{offering<-'00003'}
#' \dontrun{property<-'00060'}
#' \dontrun{obs_url<-paste(url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,sep='')}
#' \dontrun{getXMLWML1.1Data(obs_url)}
getXMLWML1.1Data <- function(obs_url){
  cat(paste("Retrieving data from: \n", obs_url, "\n", sep = " "))
  obs_open <- file(obs_url,open="r")
  content <- paste(readLines(obs_open,warn=FALSE))
  close(obs_open,type="r")
  if (length(content)>0) {
  test <- capture.output(tryCatch(xmlTreeParse(content, getDTD=F, useInternalNodes=TRUE),"XMLParserErrorList" = function(e) {cat("incomplete",e$message)}))
  } else {test<-0}
  while (length(grep("<?xml",test))==0) {
    obs_open <- file(obs_url,open="r")
    content <- paste(readLines(obs_open,warn=FALSE))
    close(obs_open,type="r")
    if (length(content)>0) {
    test <- capture.output(tryCatch(xmlTreeParse(content, getDTD=F, useInternalNodes=TRUE),"XMLParserErrorList" = function(e) {cat("incomplete",e$message)}))
    } else {
    test<-0
    }
  }
  doc <- htmlTreeParse(content, asText=TRUE, useInternalNodes=TRUE)
  values<-xpathSApply(doc, "//timeseries//value")
  values2<-sapply(values,function(x) as.numeric(xmlValue(x)))
  dateSet<-xpathSApply(doc, "//@datetime")
  dateSet2<-sapply(dateSet,function(x) toString(substr(x,1,10)))
  Daily<-as.data.frame(matrix(ncol=2,nrow=length(values2)))
  colnames(Daily)<-c('date','discharge')
  Daily$discharge<-values2
  if (length(dateSet)>2) {
    Daily$date<-as.Date(dateSet2)}
  return (Daily)
}
