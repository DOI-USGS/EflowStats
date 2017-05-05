context("mag7 stats")

test_that("mag7 stats", {
        ###load the data upfront#
        load("data/magOut.rda")
        x<-sampleData[c("date","discharge")]
        magSevenOutTest <- magnifSeven(x)
        
        expect_equal(magSevenOutTest,magSevenOut)
})


