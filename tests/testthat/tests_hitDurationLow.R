context("duration low")

test_that("duration low pref mean", {
        
        x<-sampleData[c("date","discharge")]
        
        hitDurationLowTest <- hitDurationLow(x=x,yearType="water",pref = "mean")
        hitDurationLowTestCheck <- readRDS("data/tests_hitDurationLow.rds")
        
        expect_equal(hitDurationLowTest,hitDurationLowTestCheck)
        
        x <- readRDS("data/sample_nwis_data.rds")
        hitDurationLowTest <- hitDurationLow(x=x,yearType="water",pref = "mean")
        hitDurationLowTestCheck <- readRDS("data/tests_hitDurationLow_nwis_mean.rds")
        expect_equal(hitDurationLowTest,hitDurationLowTestCheck)
        
})

test_that("duration low pref median", {
        
        x<-sampleData[c("date","discharge")]
        
        hitDurationLowTest <- hitDurationLow(x=x,yearType="water", pref = "median")
        hitDurationLowTestCheck <- readRDS("data/tests_hitDurationLow.rds")
        
        expect_equal(hitDurationLowTest,hitDurationLowTestCheck)
        
        x <- readRDS("data/sample_nwis_data.rds")
        hitDurationLowTest <- hitDurationLow(x=x,yearType="water", pref = "median")
        hitDurationLowTestCheck <- readRDS("data/tests_hitDurationLow_nwis_median.rds")
        expect_equal(hitDurationLowTest,hitDurationLowTestCheck)
})
