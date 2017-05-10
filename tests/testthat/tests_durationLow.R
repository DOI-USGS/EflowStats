context("duration low")

test_that("duration low pref mean", {
        
        x<-sampleData[c("date","discharge")]
        
        durationLowTest <- durationLow(x=x,yearType="water",pref = "mean")
        durationLowTestCheck <- readRDS("data/test_durationLow.rds")
        
        expect_equal(durationLowTest,durationLowTestCheck)
        
        x <- readRDS("data/sample_nwis_data.rds")
        durationLowTest <- durationLow(x=x,yearType="water",pref = "mean")
        durationLowTestCheck <- readRDS("data/test_durationLow_nwis_mean.rds")
        expect_equal(durationLowTest,durationLowTestCheck)
        
})

test_that("duration low pref median", {
        
        x<-sampleData[c("date","discharge")]
        
        durationLowTest <- durationLow(x=x,yearType="water", pref = "median")
        durationLowTestCheck <- readRDS("data/test_durationLow.rds")
        
        expect_equal(durationLowTest,durationLowTestCheck)
        
        x <- readRDS("data/sample_nwis_data.rds")
        durationLowTest <- durationLow(x=x,yearType="water", pref = "median")
        durationLowTestCheck <- readRDS("data/test_durationLow_nwis_median.rds")
        expect_equal(durationLowTest,durationLowTestCheck)
})
