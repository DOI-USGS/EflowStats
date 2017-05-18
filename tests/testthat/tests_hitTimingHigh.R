context("timing high")

test_that("null flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        hitTimingHighOutTest <- hitTimingHigh(x=x,yearType="water")
        hitTimingHighOut <- readRDS("data/tests_hitTimingHigh.rds")
        
        expect_equal(hitTimingHighOutTest,hitTimingHighOut)
})

test_that("numeric flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        hitTimingHighOutTest <- hitTimingHigh(x=x,yearType="water", floodThreshold=1158)
        hitTimingHighOut <- readRDS("data/tests_hitTimingHigh_1158Thresh.rds")
        
        expect_equal(hitTimingHighOutTest,hitTimingHighOut)
})