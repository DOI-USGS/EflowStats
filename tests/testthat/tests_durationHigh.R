context("duration high")

test_that("duration high", {
        ###load the data upfront#
        load("data/sampleData.rda")
        load("data/durationHighOut.rda")
        x<-sampleData[c("date","discharge")]
        durationHighOutTest <- durationHigh(x=x,yearType="water")
        
        expect_equal(durationHighOutTest,durationHighOut)
})
