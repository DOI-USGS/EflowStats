context("magnitude")

test_that("magnitude average", {
        
        x<-readRDS("data/sample_nwis_data.rds")
        da <- readRDS("data/sample_nwis_da.rds")
        
        magAveTest <- magAverage(x=x,
                                 yearType="water",
                                 drainArea = da,
                                 pref = "mean")
        
        magAveTestOut <- readRDS("data/tests_magAverage.rds")
        expect_equal(magAveTest,magAveTestOut)
        
})

test_that("magnitude low", {
        
        x<-readRDS("data/sample_nwis_data.rds")
        da <- readRDS("data/sample_nwis_da.rds")
        
        magLowTest <- magLow(x=x,
                                 yearType="water",
                                 drainArea = da,
                                 pref = "mean")
        
        magLowTestOut <- readRDS("data/tests_magLow.rds")
        expect_equal(magLowTest,magLowTestOut)
        
})

test_that("magnitude high", {
        
        x<-readRDS("data/sample_nwis_data.rds")
        da <- readRDS("data/sample_nwis_da.rds")
        
        magHighTest <- magHigh(x=x,
                             yearType="water",
                             drainArea = da,
                             pref = "mean")
        
        magHighTestOut <- readRDS("data/tests_magHigh.rds")
        expect_equal(magHighTest,magHighTestOut)
        
})