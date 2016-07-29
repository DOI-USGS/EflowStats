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
#' @import dplyr
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
SiteGoF <- function(Modeled,Gaged,site_no="") {
        
        Gaged$date <- as.Date(Gaged$date)
        Modeled$date <- as.Date(Modeled$date)
        GandM <- dplyr::left_join(Gaged[c("date","discharge")],
                                  Modeled[c("date","discharge")], 
                                  by = "date")
        names(GandM) <- c("date","discharge.obs","discharge.mod")
        GandM <- dplyr::arrange(GandM, date)
        GandM <- na.omit(GandM)
        
        if(nrow(GandM) <=0)
        {
                Output <- NA
                cat("No matching discharge days available")
                return(Output)
        }
        
        #site_no <- Gaged[1,1] I am not sure what this is all about, but it does not get the site number
        nsev<-nse(GandM$discharge.obs,GandM$discharge.mod)
        nselogv<-nselog(GandM$discharge.obs,GandM$discharge.mod)
        rmsev<-rmse(GandM$discharge.obs,GandM$discharge.mod)
        rmsnev<-rmsne(GandM$discharge.obs,GandM$discharge.mod)
        rsrv<-rsr(GandM$discharge.obs,GandM$discharge.mod)
        pbiasv<-pbias(GandM$discharge.mod,GandM$discharge.obs)
        pearsonv<-cor(GandM$discharge.mod,GandM$discharge.obs,method="pearson")
        spearmanv<-cor(GandM$discharge.mod,GandM$discharge.obs,method="spearman")
        
        obs_percentiles <- quantile(GandM$discharge.obs,probs=c(0.10, 0.25, 0.50, 0.75, 0.90),na.rm=TRUE)
        
        
        obs_10_indices <- which(GandM$discharge.obs < obs_percentiles[1])
        obs_10_25_indices <- which(GandM$discharge.obs >=obs_percentiles[1] 
                                   & GandM$discharge.obs<obs_percentiles[2])
        obs_25_50_indices <- which(GandM$discharge.obs>=obs_percentiles[2]
                                   & GandM$discharge.obs<obs_percentiles[3])
        obs_50_75_indices <- which(GandM$discharge.obs>=obs_percentiles[3]  
                                   & GandM$discharge.obs<obs_percentiles[4])
        obs_75_90_indices <- which(GandM$discharge.obs>=obs_percentiles[4]  
                                   & GandM$discharge.obs<obs_percentiles[5])
        obs_90_indices <- which(GandM$discharge.obs>=obs_percentiles[5])
        nsev_90 <- nse(GandM$discharge.obs[obs_90_indices],GandM$discharge.mod[obs_90_indices])
        nsev_75_90 <- nse(GandM$discharge.obs[obs_75_90_indices],GandM$discharge.mod[obs_75_90_indices])
        nsev_50_75 <- nse(GandM$discharge.obs[obs_50_75_indices],GandM$discharge.mod[obs_50_75_indices])
        nsev_25_50 <- nse(GandM$discharge.obs[obs_25_50_indices],GandM$discharge.mod[obs_25_50_indices])
        nsev_10_25 <- nse(GandM$discharge.obs[obs_10_25_indices],GandM$discharge.mod[obs_10_25_indices])
        nsev_10 <- nse(GandM$discharge.obs[obs_10_indices],GandM$discharge.mod[obs_10_indices])
        rmsev_90 <- rmse(GandM$discharge.obs[obs_90_indices],GandM$discharge.mod[obs_90_indices])
        rmsev_75_90 <- rmse(GandM$discharge.obs[obs_75_90_indices],GandM$discharge.mod[obs_75_90_indices])
        rmsev_50_75 <- rmse(GandM$discharge.obs[obs_50_75_indices],GandM$discharge.mod[obs_50_75_indices])
        rmsev_25_50 <- rmse(GandM$discharge.obs[obs_25_50_indices],GandM$discharge.mod[obs_25_50_indices])
        rmsev_10_25 <- rmse(GandM$discharge.obs[obs_10_25_indices],GandM$discharge.mod[obs_10_25_indices])
        rmsev_10 <- rmse(GandM$discharge.obs[obs_10_indices],GandM$discharge.mod[obs_10_indices])
        rmsnev_90 <- rmsne(GandM$discharge.obs[obs_90_indices],GandM$discharge.mod[obs_90_indices])
        rmsnev_75_90 <- rmsne(GandM$discharge.obs[obs_75_90_indices],GandM$discharge.mod[obs_75_90_indices])
        rmsnev_50_75 <- rmsne(GandM$discharge.obs[obs_50_75_indices],GandM$discharge.mod[obs_50_75_indices])
        rmsnev_25_50 <- rmsne(GandM$discharge.obs[obs_25_50_indices],GandM$discharge.mod[obs_25_50_indices])
        rmsnev_10_25 <- rmsne(GandM$discharge.obs[obs_10_25_indices],GandM$discharge.mod[obs_10_25_indices])
        rmsnev_10 <- rmsne(GandM$discharge.obs[obs_10_indices],GandM$discharge.mod[obs_10_indices])
        rsrv_90 <- rsr(GandM$discharge.obs[obs_90_indices],GandM$discharge.mod[obs_90_indices])
        rsrv_75_90 <- rsr(GandM$discharge.obs[obs_75_90_indices],GandM$discharge.mod[obs_75_90_indices])
        rsrv_50_75 <- rsr(GandM$discharge.obs[obs_50_75_indices],GandM$discharge.mod[obs_50_75_indices])
        rsrv_25_50 <- rsr(GandM$discharge.obs[obs_25_50_indices],GandM$discharge.mod[obs_25_50_indices])
        rsrv_10_25 <- rsr(GandM$discharge.obs[obs_10_25_indices],GandM$discharge.mod[obs_10_25_indices])
        rsrv_10 <- rsr(GandM$discharge.obs[obs_10_indices],GandM$discharge.mod[obs_10_indices])
        
        if (length(obs_90_indices)>1) {
                pbiasv_90 <- pbias(GandM$discharge.obs[obs_90_indices],GandM$discharge.mod[obs_90_indices])
        } else {pbiasv_90<-NA}
        if (length(obs_75_90_indices)>1) {
                pbiasv_75_90 <- pbias(GandM$discharge.obs[obs_75_90_indices],GandM$discharge.mod[obs_75_90_indices])
        } else {pbiasv_75_90<-NA}
        if (length(obs_50_75_indices)>1) {
                pbiasv_50_75 <- pbias(GandM$discharge.obs[obs_50_75_indices],GandM$discharge.mod[obs_50_75_indices])
        } else {pbiasv_50_75<-NA}
        if (length(obs_25_50_indices)>1) {
                pbiasv_25_50 <- pbias(GandM$discharge.obs[obs_25_50_indices],GandM$discharge.mod[obs_25_50_indices])
        } else {pbiasv_25_50<-NA}
        if (length(obs_10_25_indices)>1) {
                pbiasv_10_25 <- pbias(GandM$discharge.obs[obs_10_25_indices],GandM$discharge.mod[obs_10_25_indices])
        } else {pbiasv_10_25<-NA}
        if (length(obs_10_indices)>1) {
                pbiasv_10 <- pbias(GandM$discharge.obs[obs_10_indices],GandM$discharge.mod[obs_10_indices])
        } else {pbiasv_10<-NA}
        
        pearsonv_90 <- cor(GandM$discharge.obs[obs_90_indices],GandM$discharge.mod[obs_90_indices],method="pearson")
        pearsonv_75_90 <- cor(GandM$discharge.obs[obs_75_90_indices],GandM$discharge.mod[obs_75_90_indices],method="pearson")
        pearsonv_50_75 <- cor(GandM$discharge.obs[obs_50_75_indices],GandM$discharge.mod[obs_50_75_indices],method="pearson")
        pearsonv_25_50 <- cor(GandM$discharge.obs[obs_25_50_indices],GandM$discharge.mod[obs_25_50_indices],method="pearson")
        pearsonv_10_25 <- cor(GandM$discharge.obs[obs_10_25_indices],GandM$discharge.mod[obs_10_25_indices],method="pearson")
        pearsonv_10 <- cor(GandM$discharge.obs[obs_10_indices],GandM$discharge.mod[obs_10_indices],method="pearson")
        spearmanv_90 <- cor(GandM$discharge.obs[obs_90_indices],GandM$discharge.mod[obs_90_indices],method="spearman")
        spearmanv_75_90 <- cor(GandM$discharge.obs[obs_75_90_indices],GandM$discharge.mod[obs_75_90_indices],method="spearman")
        spearmanv_50_75 <- cor(GandM$discharge.obs[obs_50_75_indices],GandM$discharge.mod[obs_50_75_indices],method="spearman")
        spearmanv_25_50 <- cor(GandM$discharge.obs[obs_25_50_indices],GandM$discharge.mod[obs_25_50_indices],method="spearman")
        spearmanv_10_25 <- cor(GandM$discharge.obs[obs_10_25_indices],GandM$discharge.mod[obs_10_25_indices],method="spearman")
        spearmanv_10 <- cor(GandM$discharge.obs[obs_10_indices],GandM$discharge.mod[obs_10_indices],method="spearman")
        
        # NSEbyMonth <-vector(length=12)
        # NSELOGbyMonth <-vector(length=12)
        # RMSEbyMonth <-vector(length=12)
        # RMSNEbyMonth <- vector(length=12)
        # RSRbyMonth <- vector(length=12)
        # BiasbyMonth <-vector(length=12)
        # PearsonbyMonth <-vector(length=12)
        # SpearmanbyMonth <-vector(length=12)
        GandM$month <- lubridate::month(GandM$date)
        statsByMonth <- dplyr::summarize(dplyr::group_by(GandM, month),
                                         NSE = nse(discharge.obs,discharge.mod),
                                         NSELOG = nselog(discharge.obs,discharge.mod),
                                         RMSE = rmse(discharge.obs,discharge.mod),
                                         RMSNE = rmsne(discharge.obs,discharge.mod),
                                         RSR = rsr(discharge.obs,discharge.mod),
                                         Pearson = cor(discharge.obs,discharge.mod,method="pearson"),
                                         Spearman = cor(discharge.obs,discharge.mod,method="spearman"),
                                         Bias = pbias(discharge.obs,discharge.mod)
        )
        
        
        Output <- c(site_no, nsev,nselogv,rmsev,rmsnev,rsrv,pbiasv,pearsonv,spearmanv,
                    nsev_90,nsev_75_90,nsev_50_75,nsev_25_50,nsev_10_25,nsev_10,
                    rmsev_90,rmsev_75_90,rmsev_50_75,rmsev_25_50,rmsev_10_25,rmsev_10,
                    rmsnev_90,rmsnev_75_90,rmsnev_50_75,rmsnev_25_50,rmsnev_10_25,rmsnev_10,
                    rsrv_90,rsrv_75_90,rsrv_50_75,rsrv_25_50,rsrv_10_25,rsrv_10,
                    pbiasv_90,pbiasv_75_90,pbiasv_50_75,pbiasv_25_50,pbiasv_10_25,pbiasv_10,           
                    pearsonv_90,pearsonv_75_90,pearsonv_50_75,pearsonv_25_50,pearsonv_10_25,pearsonv_10,
                    spearmanv_90,spearmanv_75_90,spearmanv_50_75,spearmanv_25_50,spearmanv_10_25,spearmanv_10,
                    statsByMonth$NSE,statsByMonth$NSELOG,statsByMonth$RMSE,statsByMonth$RMSNE,statsByMonth$RSR,
                    statsByMonth$Bias,statsByMonth$Pearson,statsByMonth$Spearman)
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
        
        return(Output)
}