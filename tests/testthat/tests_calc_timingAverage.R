context("timing average")

test_that("timing average", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_timingAverageOutTest <- calc_timingAverage(x, floodThreshold = 1158)
        calc_timingAverageOut <- readRDS("data/tests_calc_timingAverage_1158Thresh.rds")
        
        expect_equal(calc_timingAverageOutTest,calc_timingAverageOut)
})


