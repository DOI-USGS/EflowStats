context("timing average")

test_that("timing average", {
        
        x<-sampleData[c("date","discharge")]
        
        hitTimingAverageOutTest <- hitTimingAverage(x, floodThreshold = 1158)
        hitTimingAverageOut <- readRDS("data/tests_hitTimingAverage_1158Thresh.rds")
        
        expect_equal(hitTimingAverageOutTest,hitTimingAverageOut)
})


