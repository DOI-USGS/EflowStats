context("HIT stats")

test_that("HIT stats", {

        x <- sampleData[c("date","discharge")]
        
        hitOutTest <- hitAllStats(x=x,yearType="water",stats="all",pref="mean",drainArea=50)
        hitOut <- readRDS("data/tests_hitAllStats.rds")
        
        expect_equal(hitOutTest,hitOut)
        
})

test_that("HIT stats med", {
        
        x <- sampleData[c("date","discharge")]
        
        hitOutTest <- hitAllStats(x=x,yearType="water",stats="all",pref="median",drainArea=50)
        hitOut <- readRDS("data/tests_hitAllStats_med.rds")
        
        for(stat in 1:nrow(hitOut)) {
                calc <- setNames(hitOutTest[stat,],c("name", hitOutTest[stat,1]))
                check <- setNames(hitOut[stat,],c("name", hitOut[stat,1]))
                expect_equal(calc,check)
        }
        
})