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

test_that("monthly averages work as expected", {
        
        start <- as.Date("1980-10-01")
        end <- as.Date("1985-09-30")
        x <- list()
        x$dates <- seq.Date(start, end, by = "day")
        x$month <- lubridate::month(x$dates) # submitting month values instead of flows.
        x <- as.data.frame(x)
        out <- magAverage(x, yearType = "water")
        # Expect the values returned to be the same as the month values.
        expect_equal(unlist(out[12,]), setNames(c("ma12", 1),c("indice", "statistic")))
        expect_equal(unlist(out[23,]), setNames(c("ma23", 12),c("indice", "statistic")))
        
})

