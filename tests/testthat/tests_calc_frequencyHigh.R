context("frequency high")

test_that("frequency high", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_frequencyHighTest <- calc_frequencyHigh(x=x,yearType="water")
        calc_frequencyHighTestOut <- readRDS("data/tests_calc_frequencyHigh.rds")
        
        expect_equal(calc_frequencyHighTest,calc_frequencyHighTestOut)
        
})

test_that("frequency high with peak", {
        
        peakValues <- readRDS("data/tests_get_peakThreshold.rds")
        x<-sampleData[c("date","discharge")]

        peaks <- get_peakThreshold(x,peakValues,.6,yearType="water")
        calc_frequencyHighTest <- calc_frequencyHigh(x=x,yearType="water", floodThreshold = peaks)
        calc_frequencyHighTestOut <- readRDS("data/tests_calc_frequencyHigh_peak.rds")
        
        expect_equal(calc_frequencyHighTest,calc_frequencyHighTestOut)
        
})

test_that("frequency high with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        peaks <- readRDS("data/sample_nwis_peaks.rds")
        
        calc_frequencyHighTest <- calc_frequencyHigh(x=x,yearType="water", floodThreshold = peaks, pref = "median")
        calc_frequencyHighTestOut <- readRDS("data/tests_calc_frequencyHigh_nwismed.rds")
        
        expect_equal(calc_frequencyHighTest,calc_frequencyHighTestOut)
        
})
