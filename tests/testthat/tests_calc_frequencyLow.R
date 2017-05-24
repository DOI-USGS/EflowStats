context("frequency low")

test_that("frequency low", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_frequencyLowTest <- calc_frequencyLow(x=x,yearType="water")
        
        calc_frequencyLowTestOut <- readRDS("data/tests_calc_frequencyLow.rds")
        expect_equal(calc_frequencyLowTest,calc_frequencyLowTestOut)
        
})

test_that("frequency Low with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        peaks <- readRDS("data/sample_nwis_peaks.rds")
        
        calc_frequencyLowTest <- calc_frequencyLow(x=x,yearType="water", pref = "median")
        calc_frequencyLowTestOut <- readRDS("data/tests_calc_frequencyLow_nwismed.rds")
        expect_equal(calc_frequencyLowTest,calc_frequencyLowTestOut)
        
})

test_that("frequency Low FL3", {
        
        x <- readRDS("data/tests_calc_frequencyLow_fl3_flowdata.rds")
        
        calc_frequencyLowTest <- calc_frequencyLow(x=x, yearType="water")
        
        expect_equal(calc_frequencyLowTest[3,2],4.25)
        
        calc_frequencyLowTest <- calc_frequencyLow(x=x, yearType="water", pref = "median")
        
        expect_equal(calc_frequencyLowTest[3,2],5)
        
})
