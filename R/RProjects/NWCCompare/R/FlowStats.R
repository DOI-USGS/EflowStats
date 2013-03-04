#' Function to run HIT/HAT statistics for a given data set
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
#' load_data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
#' qfiletempf<-read.csv(load_data,stringsAsFactors=FALSE)
#' FlowStats(qfiletempf,drain_area)
FlowStats <- function(data,drain_area) {
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
  
  mean_flow<-mean(dfcvbyyrf$meanq,na.rm=TRUE)
  med_flow<-median(dfcvbyyrf$meanq,na.rm=TRUE)
  cv_flow<-sd(dfcvbyyrf$meanq,na.rm=TRUE)/mean(dfcvbyyrf$meanq,na.rm=TRUE)
  cv_daily<-cv(data)
  ma1v<-ma1(data)
  ma2v<-ma2(data)
  ma3v<-ma3(data)
  ma4v<-unlist(ma4.11(data)[1])
  ma5v<-unlist(ma4.11(data)[2])
  ma6v<-unlist(ma4.11(data)[3])
  ma7v<-unlist(ma4.11(data)[4])
  ma8v<-unlist(ma4.11(data)[5])
  ma9v<-unlist(ma4.11(data)[6])
  ma10v<-unlist(ma4.11(data)[7])
  ma11v<-unlist(ma4.11(data)[8])
  ma12v<-ma12.23(data)[1:1,2:2]
  ma13v<-ma12.23(data)[2:2,2:2]
  ma14v<-ma12.23(data)[3:3,2:2]
  ma15v<-ma12.23(data)[4:4,2:2]
  ma16v<-ma12.23(data)[5:5,2:2]
  ma17v<-ma12.23(data)[6:6,2:2]
  ma18v<-ma12.23(data)[7:7,2:2]
  ma19v<-ma12.23(data)[8:8,2:2]
  ma20v<-ma12.23(data)[9:9,2:2]
  ma21v<-ma12.23(data)[10:10,2:2]
  ma22v<-ma12.23(data)[11:11,2:2]
  ma23v<-ma12.23(data)[12:12,2:2]
  ma24v<-ma24.35(data)[1,1]
  ma25v<-ma24.35(data)[2,1]
  ma26v<-ma24.35(data)[3,1]
  ma27v<-ma24.35(data)[4,1]
  ma28v<-ma24.35(data)[5,1]
  ma29v<-ma24.35(data)[6,1]
  ma30v<-ma24.35(data)[7,1]
  ma31v<-ma24.35(data)[8,1]
  ma32v<-ma24.35(data)[9,1]
  ma33v<-ma24.35(data)[10,1]
  ma34v<-ma24.35(data)[11,1]
  ma35v<-ma24.35(data)[12,1]
  ma36v<-unlist(ma36.40(data)[1])
  ma37v<-unlist(ma36.40(data)[2])
  ma38v<-unlist(ma36.40(data)[3])
  ma39v<-unlist(ma36.40(data)[4])
  ma40v<-unlist(ma36.40(data)[5])
  ma41v<-unlist(ma41.45(data,drain_area)[1])
  ma42v<-unlist(ma41.45(data,drain_area)[2])
  ma43v<-unlist(ma41.45(data,drain_area)[3])
  ma44v<-unlist(ma41.45(data,drain_area)[4])
  ma45v<-unlist(ma41.45(data,drain_area)[5])
  ml1v<-unlist(ml1.12(data)[1])
  ml2v<-unlist(ml1.12(data)[2])
  ml3v<-unlist(ml1.12(data)[3])
  ml4v<-unlist(ml1.12(data)[4])
  ml5v<-unlist(ml1.12(data)[5])
  ml6v<-unlist(ml1.12(data)[6])
  ml7v<-unlist(ml1.12(data)[7])
  ml8v<-unlist(ml1.12(data)[8])
  ml9v<-unlist(ml1.12(data)[9])
  ml10v<-unlist(ml1.12(data)[10])
  ml11v<-unlist(ml1.12(data)[11])
  ml12v<-unlist(ml1.12(data)[12])
  ml13v<-ml13(data)
  ml14v<-unlist(ml14.16(data)[1])
  ml15v<-unlist(ml14.16(data)[2])
  ml16v<-unlist(ml14.16(data)[3])
  ml17v<-ml17(data)
  ml18v<-ml18(data)
  ml19v<-ml19(data)
  ml20v<-ml20(data)
  ml21v<-ml21(data)
  ml22v<-ml22(data,drain_area)
  mh1v<-unlist(mh1.12(data)[1])
  mh2v<-unlist(mh1.12(data)[2])
  mh3v<-unlist(mh1.12(data)[3])
  mh4v<-unlist(mh1.12(data)[4])
  mh5v<-unlist(mh1.12(data)[5])
  mh6v<-unlist(mh1.12(data)[6])
  mh7v<-unlist(mh1.12(data)[7])
  mh8v<-unlist(mh1.12(data)[8])
  mh9v<-unlist(mh1.12(data)[9])
  mh10v<-unlist(mh1.12(data)[10])
  mh11v<-unlist(mh1.12(data)[11])
  mh12v<-unlist(mh1.12(data)[12])
  mh13v<-mh13(data)
  mh14v<-mh14(data)
  mh15v<-unlist(mh15.17(data)[1])
  mh16v<-unlist(mh15.17(data)[2])
  mh17v<-unlist(mh15.17(data)[3])
  mh18v<-mh18(data)
  mh19v<-mh19(data)
  mh20v<-mh20(data,drain_area)
  mh21v<-mh21(data)
  mh22v<-mh22(data)
  mh23v<-mh23(data)
  mh24v<-mh24(data)
  mh25v<-mh25(data)
  mh26v<-mh26(data)
  mh27v<-mh27(data)
  fl1v<-unlist(fl1.2(data)[1])
  fl2v<-unlist(fl1.2(data)[2])
  fh1v<-unlist(fh1.2(data)[1])
  fh2v<-unlist(fh1.2(data)[2])
  fh3v<-fh3(data) 
  fh4v<-fh4(data) 
  fh3v<-fh3v[1]
  fh4v<-fh4v[1]
  dl1v<-dl1(data)
  dl2v<-dl2(data)
  dl3v<-dl3(data)
  dl4v<-dl4(data)
  dl5v<-dl5(data)
  dl6v<-dl6(data)
  dl7v<-dl7(data)
  dl8v<-dl8(data)
  dl9v<-dl9(data)
  dl10v<-dl10(data)
  dl18v<-dl18(data)
  dh1v<-dh1(data)
  dh2v<-dh2(data)
  dh3v<-dh3(data)
  dh4v<-dh4(data)
  dh5v<-dh5(data)
  dh10v<-dh10(data)
  dh11v<-dh11(data)
  tl1v<-unlist(tl1.2(data)[1])
  tl2v<-unlist(tl1.2(data)[2])
  th1v<-unlist(th1.2(data)[1])
  th2v<-unlist(th1.2(data)[2])
  ra1v<-ra1(data)
  ra2v<-ra2(data)
  ra3v<-ra3(data)
  ra4v<-ra4(data)
  l7Q10v<-l7Q10(data)
  l7Q2v<-l7Q2(data)
  return_10v<-return_10(data)
  
  obs_percentiles <- quantile(data$discharge,probs=c(0.10, 0.25, 0.50, 0.75, 0.90),na.rm=TRUE)
  flow_10 <- obs_percentiles[1]
  flow_25 <- obs_percentiles[2]
  flow_50 <- obs_percentiles[3]
  flow_75 <- obs_percentiles[4]
  flow_90 <- obs_percentiles[5]
  
  
  Output<-c(mean_flow,med_flow,cv_flow,cv_daily,ma1v,ma2v,ma3v,ma4v,ma5v,ma6v,
            ma7v,ma8v,ma9v,ma10v,ma11v,ma12v,ma13v,ma14v,ma15v,ma16v,
            ma17v,ma18v,ma19v,ma20v,ma21v,ma22v,ma23v,ma24v,ma25v,ma26v,
            ma27v,ma28v,ma29v,ma30v,ma31v,ma32v,ma33v,ma34v,ma35v,ma36v,
            ma37v,ma38v,ma39v,ma40v,ma41v,ma42v,ma43v,ma44v,ma45v,ml1v,
            ml2v,ml3v,ml4v,ml5v,ml6v,ml7v,ml8v,ml9v,ml10v,ml11v,
            ml12v,ml13v,ml14v,ml15v,ml16v,ml17v,ml18v,ml19v,ml20v,ml21v,
            ml22v,mh1v,mh2v,mh3v,mh4v,mh5v,mh6v,mh7v,mh8v,mh9v,
            mh10v,mh11v,mh12v,mh13v,mh14v,mh15v,mh16v,mh17v,mh18v,mh19v,
            mh20v,mh21v,mh22v,mh23v,mh24v,mh25v,mh26v,mh27v,fl1v,fl2v,
            fh1v,fh2v,fh3v,fh4v,dl1v,dl2v,dl3v,dl4v,dl5v,dl6v,
            dl7v,dl8v,dl9v,dl10v,dl18v,dh1v,dh2v,dh3v,dh4v,dh5v,
            dh10v,dh11v,tl1v,tl2v,th1v,th2v,ra1v,ra2v,ra3v,ra4v,
            l7Q10v,l7Q2v,return_10v,flow_10,flow_25,flow_50,flow_75,flow_90)
  return(Output)
  
}