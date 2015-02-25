
install.packages(c("zoo","chron","doBy","XML","hydroGOF","lmomco","RCurl"))
install.packages("EflowStats",repos="http://usgs-r.github.com",type="source")

library(EflowStats)

# calculate stats for USGS streamgage(s)
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

# calculate stats for multiple sites in a local data directory
dataPathBase <- system.file("extdata", package="EflowStats")
dataPath <- paste(dataPathBase, "modeled", sep="/")
stats <- "magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
statsout <- ObservedStatsOtherMulti(dataPath,stats)

# plot monthly means for a daily discharge timeseries
qfiletempf<-sampleData
meanmonts<-monthlyMeanTs(qfiletempf)
plotMonthlyMean(meanmonts,'02178400')

# NWIS-local
sites <- c("02186000","02192000","02219000","02317500","02329600")
startDt <- "1990"
endDt <- "1999"
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
dataPath="C:/Users/jlthomps/Documents/R/JData/modeled/"
DiffStats <- CompareStats(stats,sites=sites,dataPath=dataPath,startDt=startDt,endDt=endDt)
stats1 <- DiffStats[[1]]
stats2 <- DiffStats[[2]]
Diffstats <- DiffStats[[3]]
RegGoFstats <- DiffStats[[4]]
GoFstats <- DiffStats[[5]]

# NWIS-NWIS
sites <- c("02186000","02192000","02219000","02317500","02329600")
startDt <- "1990"
endDt <- "1999"
startDt2 <- "2000"
endDt2 <- "2008"
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
DiffStats <- CompareStats(stats,sites=sites,startDt=startDt,endDt=endDt,startDt2=startDt2,endDt2=endDt2)

# local-local
stats <- "magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
dataPath <- paste(dataPathBase, "modeled", sep="/")
dataPath2 <- paste(dataPathBase, "observed", sep="/")
DiffStats <- CompareStats(stats,dataPath=dataPath,dataPath2=dataPath2)
