context("duration high")

test_that("duration high", {
       
        x<-sampleData[c("date","discharge")]
        
        calc_durationHighOutTest <- calc_durationHigh(x=x,yearType="water")
        calc_durationHighOut <-readRDS("data/tests_calc_durationHigh.rds")
        
        expect_equal(calc_durationHighOutTest,calc_durationHighOut)
})

test_that("duration high with peak", {
        
        peakValues <- readRDS("data/tests_get_peakThreshold.rds")
        x<-sampleData[c("date","discharge")]

        peaks <- get_peakThreshold(x,peakValues,.6,yearType="water")
        calc_durationHighOutTest <- calc_durationHigh(x=x,yearType="water", floodThreshold = peaks)
        calc_durationHighOut <- readRDS("data/tests_calc_durationHigh_peak.rds")
        
        expect_equal(calc_durationHighOutTest,calc_durationHighOut)
        
        expect_equal_to_reference(calc_durationHigh(x = x,
                                                  yearType = "water", 
                                                  pref = "median",
                                                  floodThreshold = peaks),
                                  "data/tests_calc_durationHigh_peak_med.rds")
        
})
