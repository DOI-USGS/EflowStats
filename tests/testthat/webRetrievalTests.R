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
  startdate<-"2012-09-01"
  enddate<-"2012-10-01"
  offering<-'00003'
  property<-'00060'
  obs_url<-paste0(url,sites,'&startDT=',startdate,'&endDT=',enddate,
                '&statCd=',offering,'&parameterCd=',property)

  data <- getXMLWML1.1Data(obs_url)
  
  expect_is(data$date, "Date")
  
})
