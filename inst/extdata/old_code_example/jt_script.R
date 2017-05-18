library(readr)

old_ts_file <- system.file("extdata/old_code_example/time_series_jt.txt", package = "EflowStats")
old_result_file <- system.file("extdata/old_code_example/time_series_result_jt.txt", package = "EflowStats")

# https://waterdata.usgs.gov/nwis/inventory/?site_no=02178400&agency_cd=USGS
ts <- read_delim(old_ts_file, delim = "\t")
ts$date <- as.Date(ts$date, format = "%m/%d/%Y")
ts$discharge <- as.numeric(ts$discharge)
ts <- as.data.frame(ts)

ts <- dataCheck(ts, yearType = "water")
new_result <- hitAllStats(ts, 
                          yearType = "water", 
                          stats = "all", 
                          digits = 3, 
                          pref = "mean", 
                          drainArea = 56.5) 

old_result <- read_delim(old_result_file, delim = "\t")
old_result$Index <- tolower(old_result$Index)
print(paste("All statistic names match?", all(old_result$Index == new_result$indice)))

compare <- new_result
names(compare) <- c("statistic", "new_result")
compare$old_result <- old_result$Value
compare$abs_diff <- abs(new_result$statistic - old_result$Value)
compare$perc_diff <- 100*abs(new_result$statistic - old_result$Value)/(rowMeans(cbind(new_result$statistic, old_result$Value),na.rm = TRUE))
