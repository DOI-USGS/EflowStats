context("frequency indicators")

###load the data upfront
load("data/sampleData.RData")
qfiletempf <- sampleData

test_that("fh1.2 test", {
  expect_equal(fh1.2(qfiletempf),list(fh1=16,
                                      fh2=44.19))
})

test_that("fh10 test", {
  expect_equal(fh10(qfiletempf),2)
})

test_that("fh11 test", {
  sites<-"02178400"
  peakValues<-getPeakData(sites)
  thresh<-1158.495
  
  expect_equal(fh11(qfiletempf,thresh),1.5)
})

test_that("fh3 test", {
  expect_equal(fh3(qfiletempf),14)
})
