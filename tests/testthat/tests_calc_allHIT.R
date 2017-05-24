context("HIT stats")

test_that("HIT stats", {

        expect_equal_to_reference(calc_allHIT(x=sampleData[c("date","discharge")],
                                              yearType="water",
                                              stats="all",
                                              pref="mean",
                                              drainArea=50),
                                  "data/tests_calc_allHIT.rds")
        
})

test_that("HIT stats med", {
        
        expect_equal_to_reference(calc_allHIT(x=sampleData[c("date","discharge")],
                                              yearType="water",
                                              stats="all",
                                              pref="median",
                                              drainArea=50),
                                  "data/tests_calc_allHIT_med.rds")
        
})