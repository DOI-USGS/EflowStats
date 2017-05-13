context("timing low")

test_that("null flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        hitTimingLowOutTest <- hitTimingLow(x=x,yearType="water")
        hitTimingLowOut <- readRDS("data/tests_hitTimingLow_null.rds")
       
        expect_equal(hitTimingLowOutTest,hitTimingLowOut)
})

test_that("numeric flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        hitTimingLowOutTest <- hitTimingLow(x=x,yearType="water", floodThreshold=1158)
        hitTimingLowOut <- readRDS("data/tests_hitTimingLow_1158Thresh.rds")
        
        expect_equal(hitTimingLowOutTest,hitTimingLowOut)
})