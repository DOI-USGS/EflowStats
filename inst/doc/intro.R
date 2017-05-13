## ----message=FALSE, echo=TRUE, eval=FALSE--------------------------------
#  library(dataRetrieval)
#  library(EflowStats)
#  
#  #Get some data
#  dailyQ <- readNWISdv(siteNumber = "04085427",
#  parameterCd = "00060",
#  startDate = "2000-10-01",
#  endDate = "2012-9-30")
#  
#  #Check data for completeness
#  dailyQClean <- dataCheck(dailyQ[c("Date","X_00060_00003")],yearType="water")
#  
#  #Get drainage area
#  siteInfo <- readNWISsite(siteNumber = "04085427")
#  drainageArea <- siteInfo$drain_area_va
#  
#  #Get peak flows
#  peakFlows <- readNWISpeak(siteNumber = "04085427",
#  startDate = "2000-10-01",
#  endDate = "2012-9-30")
#  
#  #Get flood recurence threshold
#  floodThresh <- peakThreshold(dailyQClean[c("date","discharge")],
#  peakFlows[c("peak_dt","peak_va")])
#  
#  #Calculate all hit stats
#  hitAllStatsOut <- hitAllStats(dailyQClean,
#  drainArea=drainageArea,
#  floodThreshold=floodThresh)
#  
#  #Calculate mag7 stats
#  magnifStatsOut <- magnifSeven(dailyQClean,yearType="water",digits=3)
#  

