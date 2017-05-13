context("rate change")

test_that("rate change mean", {
        
        x<-sampleData[c("date","discharge")]
        
        hitRateChangeTest <- hitRateChange(x=x,yearType="water",pref = "mean")
        
        hitRateChangeTestOut <- readRDS("data/tests_hitRateChange.rds")
        expect_equal(hitRateChangeTest,hitRateChangeTestOut)
        
})


test_that("rate change with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        
        hitRateChangeTest <- hitRateChange(x=x,yearType="water",pref = "median")
        
        hitRateChangeTestOut <- readRDS("data/tests_hitRateChange_nwismed.rds")
        expect_equal(hitRateChangeTest,hitRateChangeTestOut)
        
})
