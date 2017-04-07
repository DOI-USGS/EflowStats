# wps.des: id=nwc_stats_observed, title = Observed Daily Flow Statistics, abstract = Calculates a suite of daily flow statistics;
# wps.in: sites, string, NWIS Sites, A comma seperated list of NWIS site ids;
# wps.in: startdate, string, Start Date, The start date for analysis;
# wps.in: enddate, string, End Date, The end date for analysis;
# wps.in: stats, string, Statistic Groups, A list of statistic groups chosen from magnifSeven magStat flowStat durStat timStat rateStat;

library(EflowStats)
library(dataRetrieval)

## Inputs: uncomment for non Rserve execuation. ##
sites <- '02177000' # only works with one for now.
startdate <- "2008-10-01"
enddate <- "2013-09-30"
stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat"
## end inputs ##

# Need to get the stats list created from: "magAverage", "magLow", "magHigh", "frequencyLow", "frequencyHigh", "durationLow", "durationHigh", "timingAverage", "timingLow", "timingHigh", "rateChange"
stats <- "all" # for now

sites<-read.csv(header=F,colClasses=c("character"),text=sites)

dailyQ <- dataRetrieval::readNWISdv(siteNumber = sites[[1]],
                                    parameterCd = "00060",
                                    startDate = startdate,
                                    endDate = enddate)
dailyQClean <- dataCheck(dailyQ[c("Date","X_00060_00003")],yearType="water")

siteInfo <- readNWISsite(siteNumber = "04085427")
drainageArea <- siteInfo$drain_area_va

peakFlows <- readNWISpeak(siteNumber = sites[[1]],
                          startDate = startdate,
                          endDate = enddate)

floodThresh <- peakThreshold(dailyQClean[c("date","discharge")],
                             peakFlows[c("peak_dt","peak_va")])

statsTemp <- hitStats(dailyQClean,
                        drainArea=drainageArea,
                        floodThreshold=floodThresh)

statsTemp <- rbind(magnifSeven(dailyQClean,yearType="water",digits=3), statsTemp)
statsTemp[1:7,1] <- c("lam1Obs","tau2Obs","tau3Obs","tau4Obs","ar1Obs","amplitudeObs","phaseObs")

statsout <- data.frame(matrix(NA, nrow=length(sites), ncol = (3 + nrow(statsTemp) + 1)))
names(statsout) <- c("site_no", "min_date", "max_date", statsTemp[,1], "comment")

statsout[1,1] <- sites[[1]]
statsout[1,2] <- startdate # make sure this is the same as came back from the data?
statsout[1,3] <- enddate # make sure this is the same as came back from the data?
statsout[1,4:(length(statsout)-1)] <- statsTemp[,2]
statsout[1,length(statsout)] <- ""

output = "output.txt"

write.table(statsout, file = output, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")

# wps.out: output, text, Output File, A text file containing the table of statistics as well as monthly stats and graphs for each site;
