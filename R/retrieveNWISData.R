#' Function to return data from the NWISWeb rdb daily values service
#' 
#' This function accepts a url parameter that already contains the desired 
#' NWIS site, parameter code, statistic, startdate and enddate. It returns a 
#' data frame containing "Date" and "Discharge"
#' 
#' @param obs_url string containing the url for the retrieval
#' @return Daily a data frame containing columns 'Date' and 'Discharge'
#' @export
#' @examples
#' url<-"http://waterservices.usgs.gov/nwis/dv/?format=rdb,1.0&sites="
#' sites<-"02177000"
#' startdate<-"2012-09-01"
#' enddate<-"2012-10-01"
#' offering<-'00003'
#' property<-'00060'
#' obs_url<-paste(url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,sep='')
#' retrieveNWISData(obs_url)
retrieveNWISData <-
  function(obs_url){
    tmp <- read.delim(obs_url,header = TRUE, quote="\"", dec=".", sep='\t', colClasses=c('character'), fill = TRUE, comment.char="#")
    dataType <- tmp[1,]
    data <- tmp[-1,]
    data[,regexpr('d$',dataType) > 0] <- as.Date(data[,regexpr('d$', dataType) > 0])
    tempDF <- data[,which(regexpr('n$', dataType) > 0)]
    tempDF <- suppressWarnings(sapply(tempDF, function(x) as.numeric(x)))
    data[,which(regexpr('n$', dataType) > 0)] <- tempDF
    row.names(data) <- NULL 
    Daily <- data[, 3:4]
    colnames(Daily) <- c('date','discharge')
    return (Daily)
  }