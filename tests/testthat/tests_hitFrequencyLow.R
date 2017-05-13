context("frequency low")

test_that("frequency low", {
        
        x<-sampleData[c("date","discharge")]
        
        hitFrequencyLowTest <- hitFrequencyLow(x=x,yearType="water")
        
        hitFrequencyLowTestOut <- readRDS("data/tests_hitFrequencyLow.rds")
        expect_equal(hitFrequencyLowTest,hitFrequencyLowTestOut)
        
})

test_that("frequency Low with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        peaks <- readRDS("data/sample_nwis_peaks.rds")
        
        hitFrequencyLowTest <- hitFrequencyLow(x=x,yearType="water", pref = "median")
        hitFrequencyLowTestOut <- readRDS("data/tests_hitFrequencyLow_nwismed.rds")
        expect_equal(hitFrequencyLowTest,hitFrequencyLowTestOut)
        
})
