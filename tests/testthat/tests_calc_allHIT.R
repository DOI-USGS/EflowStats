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

test_that("bug report #183", {
  
  f <- system.file("extdata/G04135700_IHA.csv", package = "EflowStats")

  x <- read.csv(f, header=TRUE)
  
  x$date <- as.Date(x$date, "%Y-%m-%d")  
  x$discharge <- as.integer(x$discharge)
  
  expect_warning(expect_error(all_x <- calc_allHIT(x = x,
                                                   yearType = "water", 
                                                   stats = "all",
                                                   pref="mean")))
  
  dates <- seq(x$date[1], as.Date("2019-09-30", "%Y-%m-%d"), by = "day")
  
  y <- data.frame(date = dates, 
                  discharge = approx(x$date, x$discharge, 
                                     dates, method = "constant")$y)
  
  expect_equal_to_reference(calc_allHIT(x = y,
                                        yearType = "water", 
                                        stats = "all",
                                        pref="mean"), 
                            "data/tests_calc_allHIT_bug.rds")
  
  
})