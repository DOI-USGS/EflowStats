context("timing average")

test_that("timing average", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_timingAverageOutTest <- calc_timingAverage(x, floodThreshold = 1158)
        #calc_timingAverageOut <- readRDS("data/tests_calc_timingAverage_1158Thresh.rds")
        expect_equal_to_reference(calc_timingAverageOutTest,"data/tests_calc_timingAverage_1158Thresh.rds")
})

test_that("timing average works with very low flows", {
  
  x<-sampleData[c("date","discharge")]
  
  x$discharge <- x$discharge/1000
  calc_timingAverageOutTest <- calc_timingAverage(x, floodThreshold = 1)
  
  expect_equal(calc_timingAverageOutTest$statistic,c(0.305,80.359, 0.5))
  
})

