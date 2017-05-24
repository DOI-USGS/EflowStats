context("event duration")

test_that("event duration", {
        
        x<-sampleData$discharge
        
        threshold<-median(x, na.rm=TRUE)
        find_eventDurationOutTest <- find_eventDuration(x, threshold)
        
        expect_equal(find_eventDurationOutTest,11.7741935483871)
})

test_that("event duration trim", {
        
        x <- readRDS("data/sample_nwis_data.rds")$discharge
        x <- x[100:915] # gets an event at the start and end.
        threshold<-median(x, na.rm=TRUE)
        find_eventDurationOutTest <- find_eventDuration(x, threshold, trim = TRUE, aggType = "min")
        find_eventDurationOut <- readRDS("data/tests_find_eventDuration_trim.rds")
        expect_equal(find_eventDurationOutTest,find_eventDurationOut)
})
