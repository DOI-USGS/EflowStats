context("dataCheck")

test_that("dataCheck missing warning", {
        flow_data <- readRDS("data/tests_dataCheck_missing.rds")
        expect_warning(dataCheck(flow_data[3:4],yearType="water"), 
                       "Every year as defined by the yearType argument must be complete, \n                    the following years have missing data: 1999,2007")
})

test_that("dataCheck na warning", {
        flow_data <- readRDS("data/tests_dataCheck_na.rds")
        expect_warning(dataCheck(flow_data[3:4],yearType="water"), 
                       "dataframe x cannot contain NA values")
})

test_that("dataCheck date", {
        flow_data <- readRDS("data/tests_dataCheck_missing.rds")
        flow_data[3] <- as.numeric(flow_data[3][[1]])
        expect_warning(dataCheck(flow_data[3:4],yearType="water"), 
                       "First column of x must contain a vector of class date.")
        flow_data <- readRDS("data/tests_dataCheck_missing.rds")
        flow_data[4] <- as.character(flow_data[4][[1]])
        expect_warning(dataCheck(flow_data[3:4],yearType="water"), 
                       "Second column of x must contain a vector of class numeric.")
        flow_data[3] <- as.numeric(flow_data[3][[1]])
        expect_warning(dataCheck(flow_data[3:4],yearType="water"), 
                       "First column of x must contain a vector of class date.\nSecond column of x must contain a vector of class numeric.")      
})


test_that("dataCheck working", {
        flow_data <- readRDS("data/tests_dataCheck_working.rds")
        out <- dataCheck(flow_data[3:4],yearType="water")
        expect_equal(sum(out$discharge), 46003.2)
        expect_equal(length(out), 4)
        expect_equal(names(out), c("date", "discharge", "year_val", "day"))
        expect_equal(out$year_val[length(out$year_val)], 1987)
        flow_data <- flow_data[93:3744,]
        out <- dataCheck(flow_data[3:4],yearType="calendar")
        expect_equal(sum(out$discharge), 41940.2)
        expect_equal(out$year_val[length(out$year_val)], 1986)
})

test_that("dataCheck yeartype", {
        flow_data <- readRDS("data/tests_dataCheck_working.rds")
        expect_warning(dataCheck(flow_data[3:4],yearType="blah"),
                       "yearType must be one of either 'water' or 'calendar'")
})