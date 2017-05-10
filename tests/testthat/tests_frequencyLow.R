context("frequency low")

test_that("frequency low", {
        
        x<-sampleData[c("date","discharge")]
        
        frequencyLowTest <- frequencyLow(x=x,yearType="water")
        
        frequencyLowTestOut <- readRDS("data/test_frequencyLow.rds")
        expect_equal(frequencyLowTest,frequencyLowTestOut)
        
})

test_that("frequency Low with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        peaks <- readRDS("data/sample_nwis_peaks.rds")
        
        frequencyLowTest <- frequencyLow(x=x,yearType="water", pref = "median")
        frequencyLowTestOut <- readRDS("data/test_frequencyLow_nwismed.rds")
        expect_equal(frequencyLowTest,frequencyLowTestOut)
        
})
