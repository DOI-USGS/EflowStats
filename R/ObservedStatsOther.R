#' Function to return all flow statistics for an outside daily discharge data set
#' 
#' This function accepts a data frame of outside discharge data (datetime in format 'YYYY-MM-DD')
#' and list of desired statistic groups and returns a data frame containing requested statistics
#' 
#' @param daily_data data frame containing datetime and discharge
#' @param drain_area value of drainage area for station
#' @param stats string containing desired groups of statistics 
#' @return statsout data frame containing requested statistics for each station
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ObservedStatsOther(qfiletempf,stats)
ObservedStatsOther <- function(daily_data,drain_area,stats) {
  Flownum <- (length(grep("magStat", stats)) * 94) + (length(grep("flowStat", stats)) * 13) + (length(grep("durStat", stats)) * 41) + (length(grep("timStat", stats)) *                                                                                                                                         6) + (length(grep("rateStat", stats)) * 9) + (length(grep("otherStat", stats)) * 12)
  Magnifnum <- (length(grep("magnifSeven", stats)) * 7)
  colnames(daily_data) <- c("datetime","discharge")
  selqfile<-daily_data
  tempdatafr<-NULL
  tempdatafr<-data.frame(selqfile,stringsAsFactors=FALSE)
  month_val<-rep(0,length(tempdatafr$date))
  year_val<-rep(0,length(tempdatafr$date))
  day_val<-rep(0,length(tempdatafr$date))
  jul_val<-rep(0,length(tempdatafr$date))
  wy_val<-rep(0,length(tempdatafr$date))
  ones_val<-rep(1,length(tempdatafr$date))
  qfiletempf<-data.frame(tempdatafr$date,tempdatafr$discharge,month_val,year_val,day_val,jul_val,wy_val,stringsAsFactors=FALSE)
  colnames(qfiletempf)<-c('date','discharge','month_val','year_val','day_val','jul_val','wy_val')
  qfiletempf$month_val<-substr(x_obs$date,6,7)
  qfiletempf$year_val<-substr(x_obs$date,1,4)
  qfiletempf$day_val<-substr(x_obs$date,9,10)
  qfiletempf$jul_val<-strptime(x_obs$date, "%Y-%m-%d")$yday+1
  qfiletempf$wy_val<-ifelse(as.numeric(qfiletempf$month_val)>=10,as.character(as.numeric(qfiletempf$year_val)+ones_val),qfiletempf$year_val) 
  
  obs_data<-qfiletempf
  min_date <- min(obs_data[which(obs_data$month_val=="10"&obs_data$day_val=="01"),]$date)
  max_date <- max(obs_data[which(obs_data$month_val=="09"&obs_data$day_val=="30"),]$date)
  obs_data <- obs_data[which(obs_data$date>=min_date&obs_data$date<=max_date),]
  
  namesMagnif <- c("lam1Obs", "tau2Obs", "tau3Obs", "tau4Obs", "ar1Obs", "amplitudeObs", "phaseObs")
  namesMagStat <- c("ma1_mean_disc", "ma2_median_disc", "ma3_mean_annual_var", "ma4", "ma5_skew", "ma6", "ma7", "ma8", "ma9", "ma10", "ma11", "ma12_jan_mean", 
                    "ma13_feb_mean", "ma14_mar_mean", "ma15_apr_mean", "ma16_may_mean", "ma17_june_mean", "ma18_july_mean", "ma19_aug_mean", "ma20_sep_mean", "ma21_oct_mean", 
                    "ma22_nov_mean", "ma23_dec_mean", "ma24_jan_var", "ma25_feb_var", "ma26_mar_var", "ma27_apr_var", "ma28_may_var", "ma29_jun_var", "ma30_july_var", "ma31_aug_var", 
                    "ma32_sep_var", "ma33_oct_var", "ma34_nov_var", "ma35_dec_var", "ma36", "ma37_var_across_months", "ma38", "ma39_monthly_std_dev", "ma40_monthly_skewness", 
                    "ma41", "ma42", "ma43", "ma44", "ma45", "ml1", "ml2", "ml3", "ml4", "ml5", "ml6", "ml7", "ml8", "ml9", "ml10", "ml11", "ml12", "ml13_min_monthly_var", "ml14_min_annual_flow", 
                    "ml15", "ml16", "ml17", "ml18", "ml19", "ml20", "ml21", "ml22", "mh1", "mh2", "mh3", "mh4", "mh5", "mh6", "mh7", "mh8", "mh9", "mh10", "mh11", "mh12", "mh13", 
                    "mh14_med_annual_max", "mh15", "mh16_high_flow_index", "mh17", "mh18", "mh19", "mh20", "mh21", "mh22", "mh23", "mh24", "mh25", "mh26_high_peak_flow", "mh27")
  namesFlowStat <- c("fl1_low_flood_pulse", "fl2_low_pulse_var", "fl3", "fh1_high_pulse_count", "fh2_high_pulse_var", "fh3_high_pulse_count_three", "fh4_high_pulse_count_seven", 
                     "fh5", "fh6", "fh7", "fh8", "fh9", "fh10")
  namesDurStat <- c("dl1_min_daily_flow", "dl2_min_3_day_avg", "dl3", "dl4_min_30_day_avg", "dl5_min_90_day_avg", "dl6_min_flow_var", "dl7", "dl8", "dl9_min_30_day_var", 
                    "dl10_min_90_day_var", "dl11", "dl12", "dl13", "dl14", "dl15", "dl16", "dl17", "dl18_zero_flow_days", "dl19", "dl20", "dh1", "dh2", "dh3", "dh4", "dh5_max_90_day_avg", 
                    "dh6", "dh7", "dh8", "dh9", "dh10_max_90_day_var", "dh11", "dh12", "dh13", "dh14", "dh15", "dh16", "dh17", "dh18", "dh19", "dh20", "dh21")
  namesTimStat <- c("ta1", "ta2", "tl1_min_flow_julian_day", "tl2_min_julian_var", "th1_max_flow_julian_day", "th2_max_julian_var")
  namesRateStat <- c("ra1_rise_rate", "ra2_rise_rate_var", "ra3_fall_rate", "ra4_fall_rate_var", "ra5", "ra6", "ra7", "ra8", "ra9")
  namesOtherStat <- c("med_flowObs", "cv_flowObs", "cv_dailyObs", "l7Q10Obs", "l7Q2Obs", "return_10Obs", "flow_10Obs", "flow_25Obs", "flow_50Obs", "flow_75Obs", 
                      "flow_90Obs", "flow_15Obs")
  
  namesFull <- c("site_no", "min_date", "max_date")
  
  yv<-as.character(min(obs_data$date))
  ymaxv<-as.character(max(obs_data$date))
  cat(paste("dates calculated for site",site,"\n",sep=" "))
  obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
  obs_count <- nrow(obs_data)
  cat(paste("dfs created for site",site,obs_count,"\n",sep=" "))
    if (Flownum>0) {
      ObsFlowStats <- FlowStatsAll(obs_data,drain_area,stats)
      cat(paste("Flow stats calculated for site",site,"\n",sep=" "))
    }
    if (Magnifnum>0) {
      magnifSevenObs <- magnifSeven(obs_data)
      cat(paste("Mag7 stats calculated for site",site,"\n",sep=" "))
    }
  comment <- ""
  statsout<-data.frame(t(c(sites,yv,ymaxv,magnifSevenObs,ObsFlowStats,comment)),stringsAsFactors=FALSE)
  
  if (Magnifnum>0) {
    namesFull <- c(namesFull,namesMagnif)
  }
  if (length(grep("otherStat",stats))>0) {
    namesFull <- c(namesFull,namesOtherStat)
  }
  if (length(grep("magStat",stats))>0) {
    namesFull <- c(namesFull,namesMagStat)
  }
  if (length(grep("flowStat",stats))>0) {
    namesFull <- c(namesFull,namesFlowStat)
  }
  if (length(grep("durStat",stats))>0) {
    namesFull <- c(namesFull,namesDurStat)
  }
  if (length(grep("timStat",stats))>0) {
    namesFull <- c(namesFull,namesTimStat)
  }
  if (length(grep("rateStat",stats))>0) {
    namesFull <- c(namesFull,namesRateStat)
  }
  namesFull <- c(namesFull,'comment')
  
  colnames(statsout)<-namesFull
  cat("statsout created and named \n")
}