dataPath <- system.file("extdata", package="EflowStats")
dataPath <- paste(dataPath, "modeled", sep="/")

stats <- "magnifSeven,magStat,flowStat,durStat,timStat,rateStat"
obsStats <- ObservedStatsOtherMulti(dataPath,stats)

context("Miscellanious function tests")


test_that("ObservedStatsOtherMulti", {
        load("data/ObservedStatsOtherMultiOut.rda")
        
        dataPath <- system.file("extdata", package="EflowStats")
        dataPath <- paste(dataPath, "modeled", sep="/")
        
        stats <- "magnifSeven,magStat,flowStat,durStat,timStat,rateStat"
        expect_equal(ObservedStatsOtherMulti(dataPath,stats),ObservedStatsOtherMultiOut,tolerance = 1e-5)
        
})

test_that("ObservedStatsOther",{
        load("data/ObservedStatsOtherOut.rda")
        
        qfiletempf<-dailyData
        drain_area<-54
        site_id <- "Test site"
        stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat"
        expect_equal(ObservedStatsOther(qfiletempf,drain_area,site_id,stats),ObservedStatsOtherOut,tolerance=1e-5)
})
