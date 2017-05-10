context("rate change")

test_that("rate change mean", {
        
        x<-sampleData[c("date","discharge")]
        
        rateChangeTest <- rateChange(x=x,yearType="water",pref = "mean")
        
        rateChangeTestOut <- readRDS("data/test_rateChange.rds")
        expect_equal(rateChangeTest,rateChangeTestOut)
        
})


test_that("rate change with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        
        rateChangeTest <- rateChange(x=x,yearType="water",pref = "median")
        
        rateChangeTestOut <- readRDS("data/test_rateChange_nwismed.rds")
        expect_equal(rateChangeTest,rateChangeTestOut)
        
})
