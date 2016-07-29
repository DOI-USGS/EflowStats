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
#' @import dplyr
#' @examples
#' load_data<-sampleData
#' load_mod<-sampleData
#' Gaged<-load_data
#' Modeled<-load_mod
#' SiteGoF(Modeled,Gaged)
SiteGoF <- function(Modeled,Gaged,site_no="") {
        
        GandM <-  dplyr::left_join(Modeled[,c("date", "discharge")], 
                                   Gaged[,c("date", "discharge")],
                                   by="date")
        
        GandM <- dplyr::rename(GandM,
                               discharge.mod = discharge.x,
                               discharge.obs = discharge.y)
        
        GandM = dplyr::arrange(GandM,date)
        
        if(all(is.na(GandM$discharge.obs) == TRUE)){ stop("No matching discharge days available") }
        #if(is.null(site_no)){ stop("No site number provided") }
        
        nsev<-nse(GandM$discharge.obs, GandM$discharge.mod)
        nselogv<-nselog(GandM$discharge.obs, GandM$discharge.mod)
        rmsev<-rmse(GandM$discharge.obs, GandM$discharge.mod)
        rmsnev<-rmsne(GandM$discharge.obs, GandM$discharge.mod)
        rsrv<-rsr(GandM$discharge.obs, GandM$discharge.mod)
        pbiasv<-pbias(GandM$discharge.mod, GandM$discharge.obs)
        pearsonv<-cor(GandM$discharge.mod, GandM$discharge.obs, method="pearson")
        spearmanv<-cor(GandM$discharge.mod, GandM$discharge.obs, method="spearman")
        
        probs.rel <- c(0.10, 0.25, 0.50, 0.75, 0.90)
        obs_percentiles <- quantile(GandM$discharge.obs, probs=probs.rel, na.rm=TRUE)
        ptile.names <- paste("obs", c("0.0",probs.rel), c(probs.rel,"1.0"), sep="_")
        GandM$ptile <- cut(GandM$discharge.obs, c(0,obs_percentiles-1, max(GandM$discharge.obs)), labels=ptile.names)
        
        ptile.res <- ptile.res <- dplyr::summarise(dplyr::group_by(GandM,ptile),
                                                   ptile.res,
                                                   nsev = nse(discharge.obs, discharge.mod),
                                                   rmsev = rmse(discharge.obs, discharge.mod),
                                                   rmsnev = rmsne(discharge.obs, discharge.mod),
                                                   rsrv = rsr(discharge.obs, discharge.mod),
                                                   pbiasv = ifelse(n() > 1, pbias(discharge.obs, discharge.mod), NA),
                                                   pearsonv = cor(discharge.obs, discharge.mod, method="pearson"),
                                                   spearmanv = cor(discharge.obs, discharge.mod, method="spearman")
        )
        ptile.res <- dplyr::arrange(ptile.res,rev(ptile))
        ptile.res <- dplyr::select(ptile.res,-ptile) 
        ptile.res <- lapply(ptile.res,as.vector)
        ptile.res <- unlist(ptile.res)
        
        GandM$month <- lubridate::month(GandM$date)
        
        month.res <- dplyr::summarise(dplyr::group_by(GandM,month),
                               nsev = nse(discharge.obs, discharge.mod),
                               nselogv = nselog(discharge.obs, discharge.mod),
                               rmsev = rmse(discharge.obs, discharge.mod),
                               rmsnev = rmsne(discharge.obs, discharge.mod),
                               rsrv = rsr(discharge.obs, discharge.mod),
                               pbiasv = ifelse(n() > 1, pbias(discharge.obs, discharge.mod), NA),
                               pearsonv = cor(discharge.obs, discharge.mod, method="pearson"),
                               spearmanv = cor(discharge.obs, discharge.mod, method="spearman")
                               ) 
        month.res <- dplyr::arrange(month.res,month)
        month.res <- dplyr::select(month.res,-month) 
        month.res <- lapply(month.res,as.vector)
        month.res <- unlist(month.res)
        
        Output <- c(site_no, nsev, nselogv, rmsev, rmsnev, rsrv, pbiasv, pearsonv, spearmanv,
                    ptile.res, month.res)
        
        names(Output) <- c("site_no","nse","nselog","rmse","rmsne","rsr","pbias","pearson","spearman","nse_90","nse_75_90","nse_50_75","nse_25_50","nse_10_25",
                           "nse_10","rmse_90","rmse_75_90","rmse_50_75","rmse_25_50","rmse_10_25","rmse_10","rmsne_90","rmsne_75_90","rmsne_50_75",
                           "rmsne_25_50","rmsne_10_25","rmsne_10","rsr_90","rsr_75_90","rsr_50_75","rsr_25_50","rsr_10_25","rsr_10","pbias_90",
                           "pbias_75_90","pbias_50_75","pbias_25_50","pbias_10_25","pbias_10","pearson_90","pearson_75_90","pearson_50_75",
                           "pearson_25_50","pearson_10_25","pearson_10","spearman_90","spearman_75_90","spearman_50_75","spearman_25_50",
                           "spearman_10_25","spearman_10","NSEbyMonthJan","NSEbyMonthFeb","NSEbyMonthMar","NSEbyMonthApr","NSEbyMonthMay",
                           "NSEbyMonthJun","NSEbyMonthJul","NSEbyMonthAug","NSEbyMonthSep","NSEbyMonthOct","NSEbyMonthNov","NSEbyMonthDec",
                           "NSELOGbyMonthJan","NSELOGbyMonthFeb","NSELOGbyMonthMar","NSELOGbyMonthApr","NSELOGbyMonthMay","NSELOGbyMonthJun",
                           "NSELOGbyMonthJul","NSELOGbyMonthAug","NSELOGbyMonthSep","NSELOGbyMonthOct","NSELOGbyMonthNov","NSELOGbyMonthDec",
                           "RMSEbyMonthJan","RMSEbyMonthFeb","RMSEbyMonthMar","RMSEbyMonthApr","RMSEbyMonthMay","RMSEbyMonthJun","RMSEbyMonthJul",
                           "RMSEbyMonthAug","RMSEbyMonthSep","RMSEbyMonthOct","RMSEbyMonthNov","RMSEbyMonthDec","RMSNEbyMonthJan","RMSNEbyMonthFeb",
                           "RMSNEbyMonthMar","RMSNEbyMonthApr","RMSNEbyMonthMay","RMSNEbyMonthJun","RMSNEbyMonthJul","RMSNEbyMonthAug",
                           "RMSNEbyMonthSep","RMSNEbyMonthOct","RMSNEbyMonthNov","RMSNEbyMonthDec","RSRbyMonthJan","RSRbyMonthFeb","RSRbyMonthMar",
                           "RSRbyMonthApr","RSRbyMonthMay","RSRbyMonthJun","RSRbyMonthJul","RSRbyMonthAug","RSRbyMonthSep","RSRbyMonthOct",
                           "RSRbyMonthNov","RSRbyMonthDec","BiasbyMonthJan","BiasbyMonthFeb","BiasbyMonthMar","BiasbyMonthApr","BiasbyMonthMay",
                           "BiasbyMonthJun","BiasbyMonthJul","BiasbyMonthAug","BiasbyMonthSep","BiasbyMonthOct","BiasbyMonthNov","BiasbyMonthDec",
                           "PearsonbyMonthJan","PearsonbyMonthFeb","PearsonbyMonthMar","PearsonbyMonthApr","PearsonbyMonthMay","PearsonbyMonthJun",
                           "PearsonbyMonthJul","PearsonbyMonthAug","PearsonbyMonthSep","PearsonbyMonthOct","PearsonbyMonthNov","PearsonbyMonthDec",
                           "SpearmanbyMonthJan","SpearmanbyMonthFeb","SpearmanbyMonthMar","SpearmanbyMonthApr","SpearmanbyMonthMay",
                           "SpearmanbyMonthJun","SpearmanbyMonthJul","SpearmanbyMonthAug","SpearmanbyMonthSep","SpearmanbyMonthOct",
                           "SpearmanbyMonthNov","SpearmanbyMonthDec")
        
        return(Output)
}