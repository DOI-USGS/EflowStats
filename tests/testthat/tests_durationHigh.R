context("duration high")

test_that("duration high", {
        
        load("data/durationHighOut.rda") #-> durationHighOut
        x<-sampleData[c("date","discharge")]
        
        durationHighOutTest <- durationHigh(x=x,yearType="water")
        
        expect_equal(durationHighOutTest,durationHighOut)
        
})

test_that("duration high with peak", {
        
        load("data/peakValues.rda") #-> peakValues
        x<-sampleData[c("date","discharge")]

        peaks <- peakThreshold(x,peakValues,.6,yearType="water")
        durationHighOutTest <- durationHigh(x=x,yearType="water", floodThreshold = peaks)
        durationHighOut <- readRDS("data/tests_durationHigh_peak.rds")
        expect_equal(durationHighOutTest,durationHighOut)
        
})
