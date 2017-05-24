context("timing high")

test_that("null flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_timingHighOutTest <- calc_timingHigh(x=x,yearType="water")
        calc_timingHighOut <- readRDS("data/tests_calc_timingHigh.rds")
        
        expect_equal(calc_timingHighOutTest,calc_timingHighOut)
})

test_that("numeric flood threshold", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_timingHighOutTest <- calc_timingHigh(x=x,yearType="water", floodThreshold=1158)
        calc_timingHighOut <- readRDS("data/tests_calc_timingHigh_1158Thresh.rds")
        
        expect_equal(calc_timingHighOutTest,calc_timingHighOut)
})

test_that("th1 specific", {
        
        expect_equal(calc_timingHigh(x=readRDS("data/tests_calc_timingHigh_xbar_gt_0.rds"),
                                   yearType="water")[1,2],
                     69.973)
        
        expect_equal(calc_timingHigh(x=readRDS("data/tests_calc_timingHigh_xbar_lt_0.rds"),
                                   yearType="water")[1,2],
                     161.122)
})