library(readr)
library(dataRetrieval)
library(EflowStats)
old_results <- read_delim("data/old_code_example/HATindices2008_tsv.txt", delim = "\t")
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
for(site_dir in list.dirs(flow_dir)) {
        flow_files <- list.files(site_dir, pattern = "*.txt")
        if(length(flow_files) > 0){
                flow_data <- importRDB1(file.path(site_dir, flow_files[1]))
                site_no <- flow_data$site_no[1]
                flow_data_clean <- dataCheck(flow_data[3:4],yearType="water")
                if(flow_data_clean == FALSE) {
                        bad_sites <- c(bad_sites, site_no)
                } else { good_sites <- c(good_sites, site_no) }
                if(length(flow_files)>1) {
                        peak_data <- importRDB1(file.path(site_dir, flow_files[2]))
                } else { peak_data <- NULL }
                
                if(!(flow_data_clean == FALSE)) {
                        if(!is.null(peak_data)) {
                                flood_thresh <- peakThreshold(flow_data_clean[c("date","discharge")],
                                                              peak_data[c("peak_dt","peak_va")])
                                new_stats <- hitStats(flow_data_clean,
                                                      drainArea=das[site_no][[1]],
                                                      floodThreshold=flood_thresh)
                        } else {
                                new_stats <- hitStats(flow_data_clean,
                                                      drainArea=das[site_no][[1]])
                        }
                        
                }
        }
}
unlink(flow_dir, recursive = TRUE)
