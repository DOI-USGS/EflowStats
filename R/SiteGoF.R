#' Function to calculate GoF statistics for given observed and modeled data sets
#' 
#' This function accepts observed and modeled data frames of daily flow data and returns a data frame of 
#' calculated GoF statistics
#' 
#' @param Modeled data frame of daily flow data
#' @param Gaged data frame of daily flow data
#' @return Output data frame of calculated statistics
#' @export
#' @import hydroGOF
#' @import zoo
#' @import chron
#' @import doBy
#' @import lmomco
#' @examples
#' load_data<-sampleData
#' load_mod<-sampleData
#' Gaged<-load_data
#' Modeled<-load_mod
#' SiteGoF(Modeled,Gaged)
SiteGoF <- function(Modeled,Gaged) {
  Gaged <- Gaged[as.Date(Gaged$date) %in% as.Date(Modeled$date),]
  Modeled <- Modeled[as.Date(Modeled$date) %in% as.Date(Gaged$date),]
  Gaged <- Gaged[order(Gaged$date),]
  Modeled <- Modeled[order(Modeled$date),]
  if (nrow(Modeled)>1) {
    site_no <- Gaged[1,1]
  nsev<-nse(Gaged$discharge,Modeled$discharge)
  nselogv<-nselog(Gaged$discharge,Modeled$discharge)
  rmsev<-rmse(Gaged$discharge,Modeled$discharge)
  rmsnev<-rmsne(Gaged$discharge,Modeled$discharge)
  rsrv<-rsr(Gaged$discharge,Modeled$discharge)
  pbiasv<-pbias(Modeled$discharge,Gaged$discharge)
  pearsonv<-cor(Modeled$discharge,Gaged$discharge,method="pearson")
  spearmanv<-cor(Modeled$discharge,Gaged$discharge,method="spearman")
  
  obs_percentiles <- quantile(Gaged$discharge,probs=c(0.10, 0.25, 0.50, 0.75, 0.90),na.rm=TRUE)
  obs_10_indices <- which(Gaged$discharge< obs_percentiles[1])
  obs_10_25_indices <- which(Gaged$discharge>=obs_percentiles[1] 
                             & Gaged$discharge<obs_percentiles[2])
  obs_25_50_indices <- which(Gaged$discharge>=obs_percentiles[2]
                             & Gaged$discharge<obs_percentiles[3])
  obs_50_75_indices <- which(Gaged$discharge>=obs_percentiles[3]  
                             & Gaged$discharge<obs_percentiles[4])
  obs_75_90_indices <- which(Gaged$discharge>=obs_percentiles[4]  
                             & Gaged$discharge<obs_percentiles[5])
  obs_90_indices <- which(Gaged$discharge>=obs_percentiles[5])
  nsev_90 <- nse(Gaged$discharge[obs_90_indices],Modeled$discharge[obs_90_indices])
  nsev_75_90 <- nse(Gaged$discharge[obs_75_90_indices],Modeled$discharge[obs_75_90_indices])
  nsev_50_75 <- nse(Gaged$discharge[obs_50_75_indices],Modeled$discharge[obs_50_75_indices])
  nsev_25_50 <- nse(Gaged$discharge[obs_25_50_indices],Modeled$discharge[obs_25_50_indices])
  nsev_10_25 <- nse(Gaged$discharge[obs_10_25_indices],Modeled$discharge[obs_10_25_indices])
  nsev_10 <- nse(Gaged$discharge[obs_10_indices],Modeled$discharge[obs_10_indices])
  rmsev_90 <- rmse(Gaged$discharge[obs_90_indices],Modeled$discharge[obs_90_indices])
  rmsev_75_90 <- rmse(Gaged$discharge[obs_75_90_indices],Modeled$discharge[obs_75_90_indices])
  rmsev_50_75 <- rmse(Gaged$discharge[obs_50_75_indices],Modeled$discharge[obs_50_75_indices])
  rmsev_25_50 <- rmse(Gaged$discharge[obs_25_50_indices],Modeled$discharge[obs_25_50_indices])
  rmsev_10_25 <- rmse(Gaged$discharge[obs_10_25_indices],Modeled$discharge[obs_10_25_indices])
  rmsev_10 <- rmse(Gaged$discharge[obs_10_indices],Modeled$discharge[obs_10_indices])
  rmsnev_90 <- rmsne(Gaged$discharge[obs_90_indices],Modeled$discharge[obs_90_indices])
  rmsnev_75_90 <- rmsne(Gaged$discharge[obs_75_90_indices],Modeled$discharge[obs_75_90_indices])
  rmsnev_50_75 <- rmsne(Gaged$discharge[obs_50_75_indices],Modeled$discharge[obs_50_75_indices])
  rmsnev_25_50 <- rmsne(Gaged$discharge[obs_25_50_indices],Modeled$discharge[obs_25_50_indices])
  rmsnev_10_25 <- rmsne(Gaged$discharge[obs_10_25_indices],Modeled$discharge[obs_10_25_indices])
  rmsnev_10 <- rmsne(Gaged$discharge[obs_10_indices],Modeled$discharge[obs_10_indices])
  rsrv_90 <- rsr(Gaged$discharge[obs_90_indices],Modeled$discharge[obs_90_indices])
  rsrv_75_90 <- rsr(Gaged$discharge[obs_75_90_indices],Modeled$discharge[obs_75_90_indices])
  rsrv_50_75 <- rsr(Gaged$discharge[obs_50_75_indices],Modeled$discharge[obs_50_75_indices])
  rsrv_25_50 <- rsr(Gaged$discharge[obs_25_50_indices],Modeled$discharge[obs_25_50_indices])
  rsrv_10_25 <- rsr(Gaged$discharge[obs_10_25_indices],Modeled$discharge[obs_10_25_indices])
  rsrv_10 <- rsr(Gaged$discharge[obs_10_indices],Modeled$discharge[obs_10_indices])
  if (length(obs_90_indices)>1) {
  pbiasv_90 <- pbias(Gaged$discharge[obs_90_indices],Modeled$discharge[obs_90_indices])
  } else {pbiasv_90<-NA}
  if (length(obs_75_90_indices)>1) {
  pbiasv_75_90 <- pbias(Gaged$discharge[obs_75_90_indices],Modeled$discharge[obs_75_90_indices])
  } else {pbiasv_75_90<-NA}
  if (length(obs_50_75_indices)>1) {
  pbiasv_50_75 <- pbias(Gaged$discharge[obs_50_75_indices],Modeled$discharge[obs_50_75_indices])
  } else {pbiasv_50_75<-NA}
  if (length(obs_25_50_indices)>1) {
  pbiasv_25_50 <- pbias(Gaged$discharge[obs_25_50_indices],Modeled$discharge[obs_25_50_indices])
  } else {pbiasv_25_50<-NA}
  if (length(obs_10_25_indices)>1) {
  pbiasv_10_25 <- pbias(Gaged$discharge[obs_10_25_indices],Modeled$discharge[obs_10_25_indices])
  } else {pbiasv_10_25<-NA}
  if (length(obs_10_indices)>1) {
  pbiasv_10 <- pbias(Gaged$discharge[obs_10_indices],Modeled$discharge[obs_10_indices])
  } else {pbiasv_10<-NA}
  pearsonv_90 <- cor(Gaged$discharge[obs_90_indices],Modeled$discharge[obs_90_indices],method="pearson")
  pearsonv_75_90 <- cor(Gaged$discharge[obs_75_90_indices],Modeled$discharge[obs_75_90_indices],method="pearson")
  pearsonv_50_75 <- cor(Gaged$discharge[obs_50_75_indices],Modeled$discharge[obs_50_75_indices],method="pearson")
  pearsonv_25_50 <- cor(Gaged$discharge[obs_25_50_indices],Modeled$discharge[obs_25_50_indices],method="pearson")
  pearsonv_10_25 <- cor(Gaged$discharge[obs_10_25_indices],Modeled$discharge[obs_10_25_indices],method="pearson")
  pearsonv_10 <- cor(Gaged$discharge[obs_10_indices],Modeled$discharge[obs_10_indices],method="pearson")
  spearmanv_90 <- cor(Gaged$discharge[obs_90_indices],Modeled$discharge[obs_90_indices],method="spearman")
  spearmanv_75_90 <- cor(Gaged$discharge[obs_75_90_indices],Modeled$discharge[obs_75_90_indices],method="spearman")
  spearmanv_50_75 <- cor(Gaged$discharge[obs_50_75_indices],Modeled$discharge[obs_50_75_indices],method="spearman")
  spearmanv_25_50 <- cor(Gaged$discharge[obs_25_50_indices],Modeled$discharge[obs_25_50_indices],method="spearman")
  spearmanv_10_25 <- cor(Gaged$discharge[obs_10_25_indices],Modeled$discharge[obs_10_25_indices],method="spearman")
  spearmanv_10 <- cor(Gaged$discharge[obs_10_indices],Modeled$discharge[obs_10_indices],method="spearman")
  
  NSEbyMonth <-vector(length=12)
  NSELOGbyMonth <-vector(length=12)
  RMSEbyMonth <-vector(length=12)
  RMSNEbyMonth <- vector(length=12)
  RSRbyMonth <- vector(length=12)
  BiasbyMonth <-vector(length=12)
  PearsonbyMonth <-vector(length=12)
  SpearmanbyMonth <-vector(length=12)
  
  for (m in 1:12) {
    if (m<10) {month <- paste("0",m,sep="")
    } else {month<-paste("",m,sep="")}
    monthobs<-subset(Gaged,Gaged$month_val==month)
    monthmod<-subset(Modeled,Modeled$month_val==month)
    monthobs <- monthobs[order(monthobs$date),]
    monthmod <- monthmod[order(monthmod$date),]
    NSEbyMonth[m] <- nse(monthobs$discharge,monthmod$discharge)
    NSELOGbyMonth[m] <- nselog(monthobs$discharge,monthmod$discharge)
    RMSEbyMonth[m] <- rmse(monthobs$discharge,monthmod$discharge)
    RMSNEbyMonth[m] <- rmsne(monthobs$discharge,monthmod$discharge)
    RSRbyMonth[m] <- rsr(monthobs$discharge,monthmod$discharge)
    if (nrow(monthmod)>1) {
    BiasbyMonth[m] <- pbias(monthobs$discharge,monthmod$discharge)
    } else {BiasbyMonth[m]<-NA}
    PearsonbyMonth[m] <- cor(monthobs$discharge,monthmod$discharge,method="pearson")
    SpearmanbyMonth[m] <- cor(monthobs$discharge,monthmod$discharge,method="spearman")
  }
  
  Output <- c(site_no, nsev,nselogv,rmsev,rmsnev,rsrv,pbiasv,pearsonv,spearmanv,
              nsev_90,nsev_75_90,nsev_50_75,nsev_25_50,nsev_10_25,nsev_10,
              rmsev_90,rmsev_75_90,rmsev_50_75,rmsev_25_50,rmsev_10_25,rmsev_10,
              rmsnev_90,rmsnev_75_90,rmsnev_50_75,rmsnev_25_50,rmsnev_10_25,rmsnev_10,
              rsrv_90,rsrv_75_90,rsrv_50_75,rsrv_25_50,rsrv_10_25,rsrv_10,
              pbiasv_90,pbiasv_75_90,pbiasv_50_75,pbiasv_25_50,pbiasv_10_25,pbiasv_10,           
              pearsonv_90,pearsonv_75_90,pearsonv_50_75,pearsonv_25_50,pearsonv_10_25,pearsonv_10,
              spearmanv_90,spearmanv_75_90,spearmanv_50_75,spearmanv_25_50,spearmanv_10_25,spearmanv_10,
              NSEbyMonth,NSELOGbyMonth,RMSEbyMonth,RMSNEbyMonth,RSRbyMonth,BiasbyMonth,PearsonbyMonth,SpearmanbyMonth)
  Output <- as.data.frame(t(Output),stringsAsFactors=FALSE)
  colnames(Output) <- c("site_no","nse","nselog","rmse","rmsne","rsr","pbias","pearson","spearman",'nse_90','nse_75_90','nse_50_75','nse_25_50','nse_10_25',
                          'nse_10','rmse_90','rmse_75_90','rmse_50_75','rmse_25_50','rmse_10_25','rmse_10','rmsne_90','rmsne_75_90','rmsne_50_75',
                          'rmsne_25_50','rmsne_10_25','rmsne_10','rsr_90','rsr_75_90','rsr_50_75','rsr_25_50','rsr_10_25','rsr_10','pbias_90',
                          'pbias_75_90','pbias_50_75','pbias_25_50','pbias_10_25','pbias_10','pearson_90','pearson_75_90','pearson_50_75',
                          'pearson_25_50','pearson_10_25','pearson_10','spearman_90','spearman_75_90','spearman_50_75','spearman_25_50',
                          'spearman_10_25','spearman_10','NSEbyMonthJan','NSEbyMonthFeb','NSEbyMonthMar','NSEbyMonthApr','NSEbyMonthMay',
                        'NSEbyMonthJun','NSEbyMonthJul','NSEbyMonthAug','NSEbyMonthSep','NSEbyMonthOct','NSEbyMonthNov','NSEbyMonthDec',
                        'NSELOGbyMonthJan','NSELOGbyMonthFeb','NSELOGbyMonthMar','NSELOGbyMonthApr','NSELOGbyMonthMay','NSELOGbyMonthJun',
                        'NSELOGbyMonthJul','NSELOGbyMonthAug','NSELOGbyMonthSep','NSELOGbyMonthOct','NSELOGbyMonthNov','NSELOGbyMonthDec',
                        'RMSEbyMonthJan','RMSEbyMonthFeb','RMSEbyMonthMar','RMSEbyMonthApr','RMSEbyMonthMay','RMSEbyMonthJun','RMSEbyMonthJul',
                        'RMSEbyMonthAug','RMSEbyMonthSep','RMSEbyMonthOct','RMSEbyMonthNov','RMSEbyMonthDec','RMSNEbyMonthJan','RMSNEbyMonthFeb',
                        'RMSNEbyMonthMar','RMSNEbyMonthApr','RMSNEbyMonthMay','RMSNEbyMonthJun','RMSNEbyMonthJul','RMSNEbyMonthAug',
                        'RMSNEbyMonthSep','RMSNEbyMonthOct','RMSNEbyMonthNov','RMSNEbyMonthDec','RSRbyMonthJan','RSRbyMonthFeb','RSRbyMonthMar',
                        'RSRbyMonthApr','RSRbyMonthMay','RSRbyMonthJun','RSRbyMonthJul','RSRbyMonthAug','RSRbyMonthSep','RSRbyMonthOct',
                        'RSRbyMonthNov','RSRbyMonthDec','BiasbyMonthJan','BiasbyMonthFeb','BiasbyMonthMar','BiasbyMonthApr','BiasbyMonthMay',
                        'BiasbyMonthJun','BiasbyMonthJul','BiasbyMonthAug','BiasbyMonthSep','BiasbyMonthOct','BiasbyMonthNov','BiasbyMonthDec',
                        'PearsonbyMonthJan','PearsonbyMonthFeb','PearsonbyMonthMar','PearsonbyMonthApr','PearsonbyMonthMay','PearsonbyMonthJun',
                        'PearsonbyMonthJul','PearsonbyMonthAug','PearsonbyMonthSep','PearsonbyMonthOct','PearsonbyMonthNov','PearsonbyMonthDec',
                        'SpearmanbyMonthJan','SpearmanbyMonthFeb','SpearmanbyMonthMar','SpearmanbyMonthApr','SpearmanbyMonthMay',
                        'SpearmanbyMonthJun','SpearmanbyMonthJul','SpearmanbyMonthAug','SpearmanbyMonthSep','SpearmanbyMonthOct',
                        'SpearmanbyMonthNov','SpearmanbyMonthDec')
} else {
    Output <- NA
    cat("No matching discharge days available")
  }
  return(Output)
}