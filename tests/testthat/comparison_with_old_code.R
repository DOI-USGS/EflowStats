library(readr)
library(dataRetrieval)
library(EflowStats)

old_results <- suppressMessages(as.data.frame(read_delim("data/old_code_example/HATindices2008_tsv.txt", 
                                        delim = "\t", na = "NC"), 
                             stringsAsFactors = FALSE))

old_results[,1] <- as.character(gsub(" observed", "", old_results[,1]))

row.names(old_results) <- old_results[,1]
colnames(old_results) <- tolower(names(old_results))
old_results["site"] <- NULL

new_results <- old_results
new_results[1:nrow(new_results), 1:ncol(new_results)] <- NA

flow_dir <- "data/old_code_example/flow_data"

unzip("data/old_code_example/ObservedStreamflowByBasin.zip",exdir = flow_dir, overwrite = TRUE)

bad_sites <- list()
good_sites <- list()

# das <- list()
# for(site in list.dirs(flow_dir, full.names = FALSE)) {
#         if(nchar(site)>0) {
#                 das[site] <- readNWISsite(siteNumber = site)$drain_area_va
#         }
# }
# saveRDS(das, "data/old_code_example/drainage_areas.rds")

das <- readRDS("data/old_code_example/drainage_areas.rds")
out_dataset <- list()
for(site_dir in list.dirs(flow_dir)) {
        flow_files <- list.files(site_dir, pattern = "*.txt")
        if(length(flow_files) > 0){
                flow_data <- suppressMessages(importRDB1(file.path(site_dir, flow_files[1])))
                site_no <- flow_data$site_no[1]
                flow_data_clean <- dataCheck(flow_data[3:4],yearType="water")
                if(flow_data_clean == FALSE) {
                        bad_sites <- c(bad_sites, site_no)
                } else { good_sites <- c(good_sites, site_no) }
                if(length(flow_files)>1) {
                        peak_data <- suppressMessages(importRDB1(file.path(site_dir, flow_files[2])))
                } else { peak_data <- NULL }
                
                if(!(flow_data_clean == FALSE)) {
                        if(!is.null(peak_data)) {
                                flood_thresh <- peakThreshold(flow_data_clean[c("date","discharge")],
                                                              peak_data[c("peak_dt","peak_va")])
                                new_results[site_no,] <- hitStats(flow_data_clean,
                                                      drainArea=das[site_no][[1]],
                                                      floodThreshold=flood_thresh)$statistic
                        } else {
                                new_results[site_no,] <- hitStats(flow_data_clean,
                                                      drainArea=das[site_no][[1]])$statistic
                        }
                        
                }
        }
}

unlink(flow_dir, recursive = TRUE)

differences <- data.frame(matrix(ncol = length(good_sites), nrow = ncol(old_results)))
row.names(differences) <- names(old_results)
colnames(differences) <- good_sites
percent_differences <- differences

for(statN in names(old_results)) {
        for(site in good_sites) {
                try(
                        differences[statN,site] <- 
                                old_results[site,statN] - new_results[site,statN], silent = TRUE
                        )
                try(
                        percent_differences[statN,site] <- 
                                round(100*((old_results[site,statN] - new_results[site,statN]) / 
                                mean(old_results[site,statN], new_results[site,statN])),digits = 0), silent = TRUE
                )
        }
        percdiff <- mean(unlist(percent_differences[statN,]),na.rm = TRUE)
        if( !is.nan(percdiff) && percdiff > 0.1) {
                print(paste(statN, "found to be", percdiff, "percent different"))
        }
}
