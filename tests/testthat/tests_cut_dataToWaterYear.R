test_that("cut water year", {
  x <- sampleData[, c('date', 'discharge')]
  
  cut <- cut_dataToWaterYear(x, 8)
  
  expect_equal(cut$date[1], structure(15187, class = "Date"))  
  
  cut$year_val <- get_waterYear(cut$date, 8)
  
  expect_equal(cut$year_val[1], 2012)
  
  expect_true(as.Date("2012-02-29") %in% cut$date)
  
  expect_equal(nrow(cut), 366)
  
  cut <- cut_dataToWaterYear(x, 11)
  
  expect_equal(cut$date[1], structure(14914, class = "Date"))  
  
  cut$year_val <- get_waterYear(cut$date, 11)
  
  expect_equal(cut$year_val[1], 2011)
  
  expect_equal(nrow(cut), 365)
})
