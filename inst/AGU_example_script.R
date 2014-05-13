
install.packages(c("zoo","chron","doBy","XML","hydroGOF","lmomco","RCurl"))
install.packages("EflowStats",repos="http://usgs-r.github.com",type="source")

library(EflowStats)

# calculate stats for a USGS streamgage
sites <- c("02177000","02178400")
startdate <- "2009"
enddate <- "2013"
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
statsout <- ObservedStatsUSGS(sites,startdate,enddate,stats)

# calculate stats for data from your own data file
drain_area=54
site_id="Test site"
daily_data<-dailyData
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
statsout <- ObservedStatsOther(daily_data,drain_area,site_id,stats)

# plot monthly means for a daily discharge timeseries
qfiletempf<-sampleData
meanmonts<-monthlyMeanTs(qfiletempf)
plotMonthlyMean(meanmonts,'02178400')