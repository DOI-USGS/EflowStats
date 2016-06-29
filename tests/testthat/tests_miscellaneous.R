context("Miscellanious function tests")


test_that("minor function tests", {
        load("data/sampleData.rda")
        load("data/qfiletempf_zeroQ.rda")
        
        qfiletempf <- sampleData
        
        expect_equal(fl3(qfiletempf),0)
        expect_equal(fl3(qfiletempf_zeroQ),5)
        
        expect_equal(skew(qfiletempf),0.2302,tolerance=1e-4)
})

test_that("monthly, mean, and median stats",{
        load("data/sampleData.rda")
        load("data/monthlyMeanTSOut.rda")
        
        qfiletempf <- sampleData
        
        expect_equal(monthlyMeanTs(qfiletempf),monthlyMeanTSOut)
        
        expect_silent(plotMonthlyMean(monthlyMeanTSOut,'02178400'))
        
        expect_equal(meanflowbyyear(qfiletempf),data.frame(Year=c(2011,2012),
                                                           meanq=c(152.1342,151.7404)),
                     tolerance = 1e-6)
        
        expect_equal(medflowbyyear(qfiletempf),data.frame(Year = c(2011,2012),
                                                          medq = c(108,136))
        )
        
})

test_that("getDataLocal",{
        
        load("data/localDataOut.rda")
        dataPath <- system.file("extdata", package="EflowStats")
        dataPath <- paste(dataPath, "modeled", sep="/")
        startdate <- "2009"
        enddate <- "2013"
        getDataLocal(dataPath,startDt=startdate,endDt=enddate)
})