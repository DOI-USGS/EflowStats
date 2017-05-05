context("peak threshold")

test_that("peak threshold", {
        ###load the data upfront#
        load("data/sampleData.rda")
        load("data/peakValues.rda")
        load("data/peakThresholdOut.rda")
        x <- sampleData[c("date","discharge")]
        peakThresholdOutTest <- peakThreshold(x,peakValues,.6,yearType="water")
        
        expect_equal(peakThresholdOutTest,peakThresholdOut)
})
