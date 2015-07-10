context("Statistic results")

test_that("Statistic tests", {
  qfiletempf<-sampleData
  
  ar1Result <-ar1(qfiletempf)
  expect_equal(ar1Result, 0.28)
  
  bfiResult <- bfi(qfiletempf)
  expect_equal(signif(bfiResult,5), c(0.24868, 0.29107))
  
  timeseries1<-sampleData
  mgSeven <- magnifSeven(timeseries1)
  expect_equal(mgSeven, c(151.94, 0.37, 0.39, 0.28, 0.28, 0.57, -1.32))
  
  load("data/ModeledFlowStats.rda")
  load("data/GagedFlowStats.rda")
  load('data/RegGoF.rda')
  RegGoF_check<-RegionalGoF(ModeledFlowStats,GagedFlowStats)
  expect_equivalent(RegGoF_check, RegGoF)
  
  load("data/Gaged.rda")
  load("data/Modeled.rda")
  load("data/SgoF.rda")
  SiteGoF_check<-SiteGoF(Modeled,Gaged)
  expect_equivalent(SiteGoF_check, SGoF)
})