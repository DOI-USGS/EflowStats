context("timing low")

test_that("null flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_timingLowOutTest <- calc_timingLow(x=x,yearType="water")
        calc_timingLowOut <- readRDS("data/tests_calc_timingLow_null.rds")
       
        expect_equal(calc_timingLowOutTest,calc_timingLowOut)
})

test_that("numeric flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_timingLowOutTest <- calc_timingLow(x=x,yearType="water", floodThreshold=1158)
        calc_timingLowOut <- readRDS("data/tests_calc_timingLow_1158Thresh.rds")
        
        expect_equal(calc_timingLowOutTest,calc_timingLowOut)
})