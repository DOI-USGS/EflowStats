context("duration high")

test_that("duration high", {
       
        x<-sampleData[c("date","discharge")]
        
        hitDurationHighOutTest <- hitDurationHigh(x=x,yearType="water")
        hitDurationHighOut <-readRDS("data/tests_hitDurationHigh.rds")
        
        expect_equal(hitDurationHighOutTest,hitDurationHighOut)
})

test_that("duration high with peak", {
        
        peakValues <- readRDS("data/tests_peakThreshold.rds")
        x<-sampleData[c("date","discharge")]

        peaks <- peakThreshold(x,peakValues,.6,yearType="water")
        hitDurationHighOutTest <- hitDurationHigh(x=x,yearType="water", floodThreshold = peaks)
        hitDurationHighOut <- readRDS("data/tests_hitDurationHigh_peak.rds")
        
        expect_equal(hitDurationHighOutTest,hitDurationHighOut)
        
})
