context("peak threshold")

test_that("peak threshold", {
        
        peakValues <- readRDS("data/tests_get_peakThreshold.rds")
        
        x <- sampleData[c("date","discharge")]
        get_peakThresholdOutTest <- get_peakThreshold(x,peakValues,.6,yearType="water")
        
        x <- readRDS("data/sample_nwis_data.rds")
        x <- dplyr::arrange(x, date)
        x_cy <- x[93:1553,]
        peakValues <- readRDS("data/sample_nwis_peakFlows.rds")
        expect_equal(get_peakThreshold(x_cy,peakValues,.6,yearType="calendar"), 493.878871511825)
})

test_that("peak threshold multi year", {
        
        peakValues <- readRDS("data/tests_get_peakThreshold.rds")
        
        peakValues$peak_dt[52] <- "2015-04-20" #messing up the data on purpose
        x <- sampleData[c("date","discharge")]
        
        expect_warning(get_peakThresholdOutTest <-get_peakThreshold(x,peakValues,.6,yearType="water"), 
                       "peakValues data frame contains multiple peak values for one or more years. Only the maximum annual value will be retained.")
        
        get_peakThresholdOutTest <- readRDS("data/tests_get_peakThreshold_multiyear.rds")
        
        expect_equal(get_peakThresholdOutTest,1499.97261553347)
        
})

test_that("peak threshold errors are as expected", {
        peakValues <- readRDS("data/tests_get_peakThreshold.rds")
        
        x <- sampleData[c("date","discharge")]
        get_peakThresholdOutTest <- get_peakThreshold(x,peakValues,.6,yearType="water")
        
        peakValues_trunc <- peakValues[1,]
        
        expect_error(get_peakThreshold(x,peakValues_trunc,.6,yearType="water"), 
                     "peakValues must have a minimum of two annual values")
        
        peakValues_messed <- data.frame(list(peak_dt = as.character(peakValues$peak_dt), 
                                             discharge = peakValues$peak_va))
        
        expect_error(get_peakThreshold(x,peakValues_messed,.6,yearType="water"),
                     "First column of peakValues must contain a vector of class date.")
        
        peakValues_messed <- data.frame(list(peak_dt = peakValues$peak_dt, 
                                             discharge = as.character(peakValues$peak_va)))
        
        
        
        expect_error(get_peakThreshold(x,peakValues_messed,.6,yearType="water"),
                     "Second column of peakValues must contain a vector of class numeric.")
        
        peakValues_messed <- data.frame(list(peak_dt = as.character(peakValues$peak_dt), 
                                             discharge = as.character(peakValues$peak_va)))
        
        expect_error(get_peakThreshold(x,peakValues_messed,.6,yearType="water"),
                     c(paste0("First column of peakValues must contain a vector of class date.", 
                              "\nSecond column of peakValues must contain a vector of class numeric.")))
        
        peakValues_na <- peakValues
        peakValues_na[1,2] <- NA
        
        expect_error(get_peakThreshold(x,peakValues_na,.6,yearType="water"),
                     "dataframe peakValues cannot contain NA values")  
})