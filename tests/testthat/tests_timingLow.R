context("timing low")

test_that("null flood threshold", {
        ###load the data upfront#
        load("data/timingLowOut.rda")
        x<-sampleData[c("date","discharge")]
        timingLowOutTest <- timingLow(x=x,yearType="water")
        
        expect_equal(timingLowOutTest,timingLowOut)
})

test_that("numeric flood threshold", {
        ###load the data upfront#
        load("data/timingLowOut_1158Thresh.rda")
        x<-sampleData[c("date","discharge")]
        timingLowOutTest <- timingLow(x=x,yearType="water", floodThreshold=1158)
        
        expect_equal(timingLowOutTest,timingLowOut)
})