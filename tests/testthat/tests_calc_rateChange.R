context("rate change")

test_that("rate change mean", {
        
        x<-sampleData[c("date","discharge")]
        
        calc_rateChangeTest <- calc_rateChange(x=x,yearType="water",pref = "mean")
        
        calc_rateChangeTestOut <- readRDS("data/tests_calc_rateChange.rds")
        expect_equal(calc_rateChangeTest,calc_rateChangeTestOut)
        
        x[10,2] <- 0
        expect_warning(calc_rateChange(x=x,yearType="water",pref = "mean"),
                       "Discharge values of 0 were found, 0 values replace with 0.01 for RA6 and RA7 calculations")
        
})


test_that("rate change with nwis median pref", {

        x <- readRDS("data/sample_nwis_data.rds")
        
        calc_rateChangeTest <- calc_rateChange(x=x,yearType="water",pref = "median")
        
        calc_rateChangeTestOut <- readRDS("data/tests_calc_rateChange_nwismed.rds")
        expect_equal(calc_rateChangeTest,calc_rateChangeTestOut)
        
})
