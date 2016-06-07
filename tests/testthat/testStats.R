context("Statistic results")

test_that("Statistic tests", {
        load("data/sampleData.RData")
        
        qfiletempf<-sampleData
        
        ar1Result <-ar1(qfiletempf)
        expect_equal(ar1Result, 0.28)
        
        bfiResult <- bfi(qfiletempf)
        expect_equal(signif(bfiResult,5), c(0.24868, 0.29107))
        
        timeseries1<-sampleData
        mgSeven <- magnifSeven(timeseries1)
        expect_equal(mgSeven, c(151.94, 0.37, 0.39, 0.28, 0.28, 0.57, -1.32))
        
})

test_that("Regional Goodness of Fit", {
        
        load("data/ModeledFlowStats.rda")
        load("data/GagedFlowStats.rda")
        load('data/RegGoF.rda')
        RegGoF<-data.matrix(RegGoF[,3:ncol(RegGoF)])
        RegGoF_check<-RegionalGoF(ModeledFlowStats,GagedFlowStats)
        RegGoF_check<-data.matrix(RegGoF_check[,3:ncol(RegGoF_check)])
        expect_equivalent(round(RegGoF_check,digits=2),round(RegGoF,digits=2))
        
})

test_that("Statistic tests", {
        
        load("data/Gaged.rda")
        load("data/Modeled.rda")
        load("data/SGoF.rda")
        SGoF<-data.matrix(SGoF[,3:ncol(SGoF)])
        SiteGoF_check<-SiteGoF(Modeled,Gaged)
        SiteGoF_check<-data.matrix(SiteGoF_check[,3:ncol(SiteGoF_check)])
        expect_equivalent(round(SiteGoF_check,digits=2), round(SGoF,digits=2))
        
})