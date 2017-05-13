context("event duration")

test_that("event duration", {
        
        x<-sampleData$discharge
        
        threshold<-median(x, na.rm=TRUE)
        eventDurationOutTest <- eventDuration(x, threshold)
        
        expect_equal(eventDurationOutTest,11.7741935483871)
})

test_that("event duration trim", {
        
        x <- readRDS("data/sample_nwis_data.rds")$discharge
        x <- x[100:915] # gets an event at the start and end.
        threshold<-median(x, na.rm=TRUE)
        eventDurationOutTest <- eventDuration(x, threshold, trim = TRUE, aggType = "min")
        eventDurationOut <- readRDS("data/tests_eventDuration_trim.rds")
        expect_equal(eventDurationOutTest,eventDurationOut)
})
