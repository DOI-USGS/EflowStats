context("timing high")

test_that("null flood threshold", {
        ###load the data upfront#
        load("data/timingHighOut.rda")
        x<-sampleData[c("date","discharge")]
        timingHighOutTest <- timingHigh(x=x,yearType="water")
        
        expect_equal(timingHighOutTest,timingHighOut)
})

test_that("numeric flood threshold", {
        ###load the data upfront#
        load("data/timingHighOut_1158Thresh.rda")
        x<-sampleData[c("date","discharge")]
        timingHighOutTest <- timingHigh(x=x,yearType="water", floodThreshold=1158)
        
        expect_equal(timingHighOutTest,timingHighOut)
})