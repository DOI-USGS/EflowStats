context("timing high")

test_that("null flood threshold", {
        
        expect_equal_to_reference(calc_timingHigh(x=sampleData[c("date","discharge")],
                                                  yearType="water"),
                                  "data/tests_calc_timingHigh.rds")
        
})

test_that("numeric flood threshold", {
        
        expect_equal_to_reference(calc_timingHigh(x=sampleData[c("date","discharge")],
                                                  yearType="water",
                                                  floodThreshold=1158),
                                  "data/tests_calc_timingHigh_1158Thresh.rds")
        
})

test_that("th1 specific", {
        
        expect_equal(calc_timingHigh(x=readRDS("data/tests_calc_timingHigh_xbar_gt_0.rds"),
                                   yearType="water")[1,2],
                     343.855)
        
        expect_equal(calc_timingHigh(x=readRDS("data/tests_calc_timingHigh_xbar_lt_0.rds"),
                                   yearType="water")[1,2],
                     69.122)
})