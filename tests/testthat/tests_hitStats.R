context("HIT stats")

test_that("HIT stats", {

        x <- sampleData[c("date","discharge")]
        
        hitOutTest <- hitStats(x=x,yearType="water",stats="all",pref="mean",drainArea=50)
        hitOut <- readRDS("data/test_hitStats.rds")
        expect_equal(hitOutTest,hitOut)
})


