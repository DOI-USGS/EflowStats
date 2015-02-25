context("Functions that call web services")

test_that("General NWIS retrievals working", {
  testthat::skip_on_cran()
  
  drainage_url<-"http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
  sites<-"02177000"
  drain_url<-paste(drainage_url,sites,sep="")

  da <- getDrainageArea(drain_url)
  expect_that(da == 207, is_true())
  
  url<-"http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
  sites<-"02177000"
  startdate<-"2012-10-01"
  enddate<-"2013-9-30"
  offering<-'00003'
  property<-'00060'
  obs_url<-paste0(url,sites,'&startDT=',startdate,'&endDT=',enddate,
                '&statCd=',offering,'&parameterCd=',property)

  data <- getXMLWML1.1Data(obs_url)
  
  expect_is(data$date, "Date")

  obs_data <- get_obsdata(data)
  expect_that(nrow(obs_data) == 365, is_true())

  sites <- c("02177000", "02178400")
  startdate <- "2009"
  enddate <- "2013"

  data <- getDataUSGS(sites,startdate,enddate)
  expect_that(length(data) == length(sites), is_true())

  startdate <- "2003"
  enddate <- "2008"
  startdate2 <- "2009"
  enddate2 <- "2013"
#   stats <- "magnifSeven,magStat,flowStat,durStat,timStat,rateStat"
  stats="durStat"

  compareResults <- CompareStats(stats,sites=sites,
                                startDt=startdate,
                                endDt=enddate,
                                startDt2=startdate2,
                                endDt2=enddate2)
  expect_that(length(compareResults) == 4, is_true())

  sites <- c("02177000", "02178400")
  startdate <- "2009"
  enddate <- "2013"
  stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat"
  
  obsStats <- ObservedStatsUSGS(sites,startdate,enddate,stats)
  expect_that(nrow(obsStats) == length(sites), is_true())

})
