context("duration low")

test_that("duration low pref mean", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_durationLowTest <- calc_durationLow(x=x,yearType="water",pref = "mean")
        calc_durationLowTestCheck <- readRDS("data/tests_calc_durationLow.rds")
        
        expect_equal(calc_durationLowTest,calc_durationLowTestCheck)
        
        x <- readRDS("data/sample_nwis_data.rds")
        calc_durationLowTest <- calc_durationLow(x=x,yearType="water",pref = "mean")
        calc_durationLowTestCheck <- readRDS("data/tests_calc_durationLow_nwis_mean.rds")
        expect_equal(calc_durationLowTest,calc_durationLowTestCheck)
        
})

test_that("duration low pref median", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_durationLowTest <- calc_durationLow(x=x,yearType="water", pref = "median")
        calc_durationLowTestCheck <- readRDS("data/tests_calc_durationLow.rds")
        
        expect_equal(calc_durationLowTest,calc_durationLowTestCheck)
        
        x <- readRDS("data/sample_nwis_data.rds")
        calc_durationLowTest <- calc_durationLow(x=x,yearType="water", pref = "median")
        calc_durationLowTestCheck <- readRDS("data/tests_calc_durationLow_nwis_median.rds")
        expect_equal(calc_durationLowTest,calc_durationLowTestCheck)
})
