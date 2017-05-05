context("event duration")

test_that("event duration", {
        ###load the data upfront#
        load("data/sampleData.rda")
        load("data/eventDurationOut.rda")
        x<-sampleData$discharge
        threshold<-median(x, na.rm=TRUE)
        eventDurationOutTest <- eventDuration(x, threshold)
        
        expect_equal(eventDurationOutTest,eventDurationOut)
})
