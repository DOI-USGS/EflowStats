# wps.des: id=nwc_stats_observed, title = Observed Daily Flow Statistics, abstract = Calculates a suite of daily flow statistics;
# wps.in: sites, string, NWIS Sites, A comma seperated list of NWIS site ids;
# wps.in: startdate, string, Start Date, The start date for analysis;
# wps.in: enddate, string, End Date, The end date for analysis;
# wps.in: stats, string, Statistic Groups, A list of statistic groups chosen from magnifSeven magStat flowStat durStat timStat rateStat;

library(EflowStats)
library(NWCCompare)
## Inputs: uncomment for non Rserve execuation. ##
# sites <- '02177000,02178400'
# startdate <- "2008-10-01"
# enddate <- "2013-09-29"
#stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat"
## end inputs ##

nwisDvUrl <- "https://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering <- "00003"
property <- "00060"
drainage_url <- "https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="

sites<-read.csv(header=F,colClasses=c("character"),text=sites)
x_urls<-paste(nwisDvUrl, sites, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property, sep = "")
d_urls<-paste(drainage_url, sites, sep = "")
statsout <- calculateStatsGroups(stats, sites, startdate, enddate, getXMLWML1.1Data, x_urls, getDrainageArea, sites)
output = "output.txt"
write.table(statsout, file = output, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")

# wps.out: output, text, Output File, A text file containing the table of statistics as well as monthly stats and graphs for each site;
