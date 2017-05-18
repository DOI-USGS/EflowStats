context("peak threshold")

test_that("peak threshold", {
        
        peakValues <- readRDS("data/tests_peakThreshold.rds")
        
        x <- sampleData[c("date","discharge")]
        peakThresholdOutTest <- peakThreshold(x,peakValues,.6,yearType="water")
        
        expect_equal(peakThresholdOutTest,1499.97261553347)
})

test_that("peak threshold multi year", {
        
        peakValues <- readRDS("data/tests_peakThreshold.rds")
        
        peakValues$peak_dt[52] <- "2015-04-20" #messing up the data on purpose
        x <- sampleData[c("date","discharge")]
        
        expect_warning(peakThresholdOutTest <-peakThreshold(x,peakValues,.6,yearType="water"), 
                       "peakValues data frame contains multiple peak values for one or more years. Only the maximum annual value will be retained.")
        
        peakThresholdOutTest <- readRDS("data/tests_peakThreshold_multiyear.rds")
        
        expect_equal(peakThresholdOutTest,1499.97261553347)
        
})
