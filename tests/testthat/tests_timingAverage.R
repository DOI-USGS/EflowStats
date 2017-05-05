context("timing average")

test_that("timing average", {
        ###load the data upfront#
        load("data/sampleData.rda")
        load("data/timingAverageOut.rda")
        x<-sampleData[c("date","discharge")]
        timingAverageOutTest <- timingAverage(x, floodThreshold = 1158)
        
        expect_equal(timingAverageOutTest,timingAverageOut)
})


