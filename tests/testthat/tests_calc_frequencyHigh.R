context("frequency high")

test_that("frequency high", {
        
        x<-sampleData[c("date","discharge")]
        
        hitFrequencyHighTest <- hitFrequencyHigh(x=x,yearType="water")
        hitFrequencyHighTestOut <- readRDS("data/tests_hitFrequencyHigh.rds")
        
        expect_equal(hitFrequencyHighTest,hitFrequencyHighTestOut)
        
})

test_that("frequency high with peak", {
        
        peakValues <- readRDS("data/tests_peakThreshold.rds")
        x<-sampleData[c("date","discharge")]

        peaks <- peakThreshold(x,peakValues,.6,yearType="water")
        hitFrequencyHighTest <- hitFrequencyHigh(x=x,yearType="water", floodThreshold = peaks)
        hitFrequencyHighTestOut <- readRDS("data/tests_hitFrequencyHigh_peak.rds")
        
        expect_equal(hitFrequencyHighTest,hitFrequencyHighTestOut)
        
})

test_that("frequency high with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        peaks <- readRDS("data/sample_nwis_peaks.rds")
        
        hitFrequencyHighTest <- hitFrequencyHigh(x=x,yearType="water", floodThreshold = peaks, pref = "median")
        hitFrequencyHighTestOut <- readRDS("data/tests_hitFrequencyHigh_nwismed.rds")
        
        expect_equal(hitFrequencyHighTest,hitFrequencyHighTestOut)
        
})
