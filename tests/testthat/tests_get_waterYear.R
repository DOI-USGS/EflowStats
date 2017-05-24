context("get_waterYear")

test_that("get_waterYear Works", {
        expect_equal(get_waterYear(as.Date("2012-10-01")), 
                     2013)
        
        expect_equal(get_waterYear(as.Date("2012-09-01")), 
                     2012)
        
        expect_equal(get_waterYear(as.Date("2012-10-01"), numeric = FALSE), 
                     factor("2013", ordered = T))
})
        