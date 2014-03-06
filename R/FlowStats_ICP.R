#' Function to run ICP HIT/HAT statistics for a given data set
#' 
#' This function accepts a data frame of daily flow data and returns a data frame of 
#' calculated HIT/HAT statistics 
#' 
#' @param data data frame of daily flow data
#' @param drain_area drainage area for a given site
#' @return Output data frame of calculated statistics
#' @export
#' @examples
#' drainage_url<-"http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
#' sites<-"02177000"
#' drain_url<-paste(drainage_url,sites,sep="")
#' drain_area<-getDrainageArea(drain_url)
#' qfiletempf<-sampleData
#' FlowStatsICP(qfiletempf,mod_data,drain_area)
FlowStatsICP <- function(data,drain_area) {
  dfOut <- vector()
  magnif7 <- magnifSeven(data)
  dfOut <- c(dfOut,magnif7)
  sdbyyr <- aggregate(data$discharge, list(data$year_val), 
                      sd)
  colnames(sdbyyr) <- c("Year", "sdq")
  meanbyyr <- aggregate(data$discharge, list(data$year_val), 
                        mean, na.rm=TRUE)
  colnames(meanbyyr) <- c("Year", "meanq")
  medbyyr <- aggregate(data$discharge, list(data$year_val), 
                       median, na.rm=TRUE)
  colnames(medbyyr) <- c("Year","medq")
  dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
                         meanbyyr$meanq, medbyyr$medq)
  colnames(dfcvbyyr) <- c("Year", "sdq", "meanq", "medq")
  cvbyyr <- dfcvbyyr$sdq/dfcvbyyr$meanq
  dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr)
  colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", "medq", 
                           "cvq")
  med_flow<-median(dfcvbyyrf$meanq,na.rm=TRUE)
  cv_flow<-sd(dfcvbyyrf$meanq,na.rm=TRUE)/mean(dfcvbyyrf$meanq,na.rm=TRUE)
  l7Q10v<-l7Q10(data)
  l7Q2v<-l7Q2(data)
  return_10v<-return_10(data)
  
  obs_percentiles <- quantile(data$discharge,probs=c(0.10, 0.25, 0.50, 0.75, 0.90, 0.15),na.rm=TRUE)
  flow_10 <- obs_percentiles[1]
  flow_25 <- obs_percentiles[2]
  flow_50 <- obs_percentiles[3]
  flow_75 <- obs_percentiles[4]
  flow_90 <- obs_percentiles[5]
  flow_15 <- obs_percentiles[6]
  dfOut <- c(dfOut,med_flow,cv_flow,l7Q10v,l7Q2v,return_10v,flow_10,flow_25,flow_50,flow_75,flow_90,flow_15)

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