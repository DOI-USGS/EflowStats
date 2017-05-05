context("HIT stats")

test_that("HIT stats", {
        ###load the data upfront#
        load("data/hitOut.rda")
        x <- sampleData[c("date","discharge")]
        hitOutTest <- hitStats(x=x,yearType="water",stats="all",pref="mean",drainArea=50)
        expect_equal(hitOutTest,hitOut)
})


