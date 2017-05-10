context("frequency high")

test_that("frequency high", {
        
        x<-sampleData[c("date","discharge")]
        
        frequencyHighTest <- frequencyHigh(x=x,yearType="water")
        
        frequencyHighTestOut <- readRDS("data/test_frequencyHigh.rds")
        expect_equal(frequencyHighTest,frequencyHighTestOut)
        
})

test_that("frequency high with peak", {
        
        load("data/peakValues.rda") #-> peakValues
        x<-sampleData[c("date","discharge")]

        peaks <- peakThreshold(x,peakValues,.6,yearType="water")
        frequencyHighTest <- frequencyHigh(x=x,yearType="water", floodThreshold = peaks)
        frequencyHighTestOut <- readRDS("data/test_frequencyHigh_peak.rds")
        expect_equal(frequencyHighTest,frequencyHighTestOut)
        
})

test_that("frequency high with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        peaks <- readRDS("data/sample_nwis_peaks.rds")
        
        frequencyHighTest <- frequencyHigh(x=x,yearType="water", floodThreshold = peaks, pref = "median")
        frequencyHighTestOut <- readRDS("data/test_frequencyHigh_nwismed.rds")
        expect_equal(frequencyHighTest,frequencyHighTestOut)
        
})
