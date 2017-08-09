context("timing low")

test_that("null flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_timingLowOutTest <- calc_timingLow(x=x,yearType="water")

        expect_equal_to_reference(calc_timingLowOutTest,"data/tests_calc_timingLow_null.rds")
})

test_that("numeric flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_timingLowOutTest <- calc_timingLow(x=x,yearType="water", floodThreshold=1158)

        expect_equal_to_reference(calc_timingLowOutTest,"data/tests_calc_timingLow_1158Thresh.rds")
})

test_that("contributed example works as expected.", {
        # This test was contributed by a user who found a bug in the tl1.
        # There is more data here than needed for this issue. 
        # It maye be useful in the future for other things.
        dvs <- readRDS("data/test_tl1plus_data.rds")
        drainageArea <- 175
        
        output <- calc_timingLow(dvs, drainageArea = 175)       
        expect_equal(output$statistic[1], 273)
}) 
