library(HITHATStats)
sos_url="http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering='00003'
property='00060'
drainage_url="http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
startdate<-"1900-01-01"
enddate<-"2012-10-01"
sites="01463500"

drain_url<-paste(drainage_url,sites,sep="")
drain_area<-getDrainageArea(drain_url)

url2<-paste(sos_url,sites,'&startDT=',startdate,'&endDT=',enddate,'&statCd=',offering,'&parameterCd=',property,'&access=3',sep='')
x_obs <- getXMLWML1.1Data(url2)
obs_data <- get_obsdata(x_obs)

ma1(obs_data)



