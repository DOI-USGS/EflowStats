context("HIT stats")

test_that("HIT stats", {

        expect_equal_to_reference(hitAllStats(x=sampleData[c("date","discharge")],
                                              yearType="water",
                                              stats="all",
                                              pref="mean",
                                              drainArea=50),
                                  "data/tests_hitAllStats.rds")
        
})

test_that("HIT stats med", {
        
        expect_equal_to_reference(hitAllStats(x=sampleData[c("date","discharge")],
                                              yearType="water",
                                              stats="all",
                                              pref="median",
                                              drainArea=50),
                                  "data/tests_hitAllStats_med.rds")
        
})