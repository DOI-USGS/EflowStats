context("HIT stats")

test_that("HIT stats", {

        x <- sampleData[c("date","discharge")]
        
        hitOutTest <- hitAllStats(x=x,yearType="water",stats="all",pref="mean",drainArea=50)
        hitOut <- readRDS("data/tests_hitAllStats.rds")
        expect_equal(hitOutTest,hitOut)
})


