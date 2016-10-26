###Bechmarking

#get long Q record
library(dataRetrieval)
?readNWISdv

siteNumber <- '04085427'
startDate <- '1980-01-01'
endDate <- '2012-06-30'
pCode <- '00060'
rawDailyQ <- readNWISdv(siteNumber,pCode, startDate, endDate)


system.time({
        magHigh(x = rawDailyQ[c("Date","X_00060_00003")],stats = "All",yearType = "water",digits=3,drainArea = NULL,pref="mean")
})

