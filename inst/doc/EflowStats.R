## ----openLibrary, echo=FALSE------------------------------
library(xtable)
options(continue=" ")
options(width=60)
library(knitr)


## ----include=TRUE ,echo=FALSE,eval=TRUE-------------------
opts_chunk$set(highlight=TRUE, tidy=TRUE, keep.space=TRUE, keep.blank.space=FALSE, keep.comment=TRUE, concordance=TRUE,tidy=FALSE,comment="")

knit_hooks$set(inline = function(x) {
   if (is.numeric(x)) round(x, 3)})
knit_hooks$set(crop = hook_pdfcrop)

bold.colHeaders <- function(x) paste("\\multicolumn{1}{c}{\\textbf{\\textsf{", x, "}}}", sep = "")
addSpace <- function(x) ifelse(x != "1", "[5pt]","")


## ----workflow, echo=TRUE,eval=TRUE------------------------
library(EflowStats)

## ----USGSstatsinfo, echo=TRUE,eval=TRUE-------------------
# calculate stats for a USGS streamgage
sites <- c("02177000","02178400")
startdate <- "2009"
enddate <- "2013"
stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"

## ----createstatsoutput, echo=FALSE,eval=TRUE,results="hide"----
# calculate stats for USGS streamgage(s)
statsout <- ObservedStatsUSGS(sites,startdate,enddate,stats)

## ----statsoutput, echo=TRUE,eval=FALSE--------------------
#  # calculate stats for USGS streamgage(s)
#  statsout <- ObservedStatsUSGS(sites,startdate,enddate,stats)

## ----viewData, echo=FALSE,eval=TRUE-----------------------
# view a portion of the statsout table
statsout[,c(1,4:8,10)]

## ----saveData, echo=TRUE,eval=FALSE-----------------------
#  # save statsout to a tab-delimited file
#  output = "output.txt"
#  write.table(statsout, file = output, col.names = TRUE,
#              row.names = FALSE, quote = FALSE, sep = "\t")

## ----loadData, echo=TRUE,eval=TRUE------------------------
# Load sample data included with package:
daily_data<-dailyData

## ----loadFile, echo=TRUE,eval=FALSE-----------------------
#  #load locally stored daily discharge data
#  dailyData <- read.table("dailyData.csv",sep=",",
#                          stringsAsFactors=FALSE,header=TRUE)

## ----OtherStats, echo=TRUE,eval=FALSE---------------------
#  # calculate stats for data from your own data file
#  drain_area=54
#  site_id="Test site"
#  daily_data<-dailyData
#  stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
#  statsout <- ObservedStatsOther(daily_data,drain_area,site_id,stats)

## ----OtherStatsMulti, echo=TRUE,eval=FALSE----------------
#  # calculate stats for multiple sites in a local data directory
#  dataPath="C:/Users/jlthomps/Documents/R/JData/modeled/"
#  stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
#  statsout <- ObservedStatsOtherMulti(dataPath,stats)

## ----plotMonthlyMeans, echo=TRUE,fig.cap="Monthly Average Flow at station 02178400"----
# plot monthly means for a daily discharge timeseries
qfiletempf<-sampleData
meanmonts<-monthlyMeanTs(qfiletempf)
plotMonthlyMean(meanmonts,'02178400')

## ----CompareStatsNWISlocal, echo=TRUE,eval=FALSE----------
#  # NWIS-local
#  sites <- c("02186000","02192000","02219000","02317500","02329600")
#  startDt <- "1990"
#  endDt <- "1999"
#  stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
#  dataPath="C:/Users/jlthomps/Documents/R/JData/modeled/"
#  DiffStats <- CompareStats(stats,sites=sites,dataPath=dataPath,
#                            startDt=startDt,endDt=endDt)
#  stats1 <- DiffStats[[1]]
#  stats2 <- DiffStats[[2]]
#  Diffstats <- DiffStats[[3]]
#  RegGoFstats <- DiffStats[[4]]
#  GoFstats <- DiffStats[[5]]

## ----CompareStatsNWISNWIS, echo=TRUE,eval=FALSE-----------
#  # NWIS-NWIS
#  sites <- c("02186000","02192000","02219000","02317500","02329600")
#  startDt <- "1990"
#  endDt <- "1999"
#  startDt2 <- "2000"
#  endDt2 <- "2008"
#  stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
#  DiffStats <- CompareStats(stats,sites=sites,startDt=startDt,
#                            endDt=endDt,startDt2=startDt2,endDt2=endDt2)

## ----CompareStatslocallocal, echo=TRUE,eval=FALSE---------
#  # local-local
#  stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
#  dataPath="C:/Users/jlthomps/Documents/R/JData/modeled/"
#  dataPath2="C:/Users/jlthomps/Documents/R/JData/observed/"
#  DiffStats <- CompareStats(stats,dataPath=dataPath,dataPath2=dataPath2)

## ----CompareStatsNWISNWISb, echo=TRUE,eval=FALSE----------
#  # NWIS-NWIS
#  sites <- c("02186000")
#  startDt <- "1990"
#  endDt <- "1999"
#  startDt2 <- "2000"
#  endDt2 <- "2008"
#  data1 <- getDataUSGS(sites,startDt,endDt)[[1]]
#  data2 <- getDataUSGS(sites,startDt2,endDt2)[[1]]
#  fh1Reference <- fh1.2(data1)[[1]]
#  fh1Urbanized <- fh1.2(data2)[[1]]
#  dh15Reference <- dh15.16(data1)[[1]]
#  dh15Urbanized <- dh15.16(data2)[[1]]
#  ra6Reference <- ra6(data1)
#  ra6Urbanized <- ra6(data2)

## ----helpFunc,eval = FALSE--------------------------------
#  library(EflowStats)
#  ?mh19

## ----rawFunc,eval = TRUE----------------------------------
mh19

## ----installFromCran,eval = FALSE-------------------------
#  install.packages("EflowStats",repos=c("http://owi.usgs.gov/R",
#                                        "http://cran.us.r-project.org"))

## ----openLibraryTest, eval=FALSE--------------------------
#  library(EflowStats)

