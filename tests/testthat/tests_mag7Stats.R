context("mag7 stats")

test_that("mag7 stats", {
        x<-sampleData[c("date","discharge")]
        
        magSevenOutTest <- magnifSeven(x)
        magSevenOut <- readRDS("data/tests_mag7Stats.rds")
        
        expect_equal(magSevenOutTest,magSevenOut)
})


