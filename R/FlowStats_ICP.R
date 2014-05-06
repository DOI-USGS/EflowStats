#' Function to run ICP HIT/HAT statistics for a given data set
#' 
#' This function accepts a data frame of daily flow data and returns a data frame of 
#' calculated HIT/HAT statistics 
#' 
#' @param data data frame of daily flow data
#' @param drain_area value of site drainage area
#' @return Output data frame of calculated statistics
#' @export
#' @examples
#' drainage_url<-"http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
#' sites<-"02177000"
#' drain_url<-paste(drainage_url,sites,sep="")
#' drain_area<-getDrainageArea(drain_url)
#' qfiletempf<-sampleData
#' qfiletempf$date <- as.Date(qfiletempf$date,"%m/%d/%y")
#' FlowStatsICP(qfiletempf,drain_area)
FlowStatsICP <- function(data,drain_area) {
  dfOut <- vector()
  otherstat <- OtherStats(data)
  dfOut <- c(dfOut,otherstat)

    ma26v<-ma24.35(data)[3,1]
    ma41v<-unlist(ma41.45(data,drain_area)[1])
    ml18v<-ml18(data)
    ml20v<-ml20(data)
    mh10v<-unlist(mh1.12(data)[10])
  dfOut <- c(dfOut,ma26v,ma41v,ml18v,ml20v,mh10v)

    fl2v<-unname(unlist(fl1.2(data)[2]))
    fh6v<-fh6(data)
    fh7v<-fh7(data)
  dfOut <- c(dfOut,fl2v,fh6v,fh7v)

    dl6v<-dl6(data)
    dl13v<-dl13(data)
    dl16v<-unname(unlist(dl16.17(data)[1]))
  dfOut <- c(dfOut,dl6v,dl13v,dl16v)

    ta1v<-unname(unlist(ta1.2(data)[1]))
    tl1v<-unname(unlist(tl1.2(data)[1]))
    th1v<-unname(unlist(th1.2(data)[1]))
  dfOut <- c(dfOut,ta1v,tl1v,th1v)

    ra5v<-ra5(data)
    ra7v<-ra7(data)
    ra8v<-unname(unlist(ra8.9(data)[1]))
  dfOut <- c(dfOut,ra5v,ra7v,ra8v)

  Output<-dfOut
  return(Output)
  
}