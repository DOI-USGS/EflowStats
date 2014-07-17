#' Function to return all flow statistics and comparison statistics for a set of USGS sites and start and end date
#' 
#' This function accepts a list of sites, one or two data paths, one or two sets of start and end dates and list of desired statistic groups and returns a data frame containing requested statistics
#' 
#' @param sites optional list of USGS station ids
#' @param dataPath path to local directory containing data files
#' @param stats string containing desired groups of statistics
#' @param startDt beginning water year, will be transalted to 10/01
#' @param endDt ending water year, will be translated to 09/30
#' @param sepChar string containing the datafile separator, default is comma
#' @param dataPath2 path to local directory containing data files
#' @param startDt2 beginning water year, will be transalted to 10/01
#' @param endDt2 ending water year, will be translated to 09/30
#' @return statsout data frame containing requested statistics for each station
#' @export
#' @examples
#' sites <- c("02177000", "02178400")
#' startdate <- "2003"
#' enddate <- "2008"
#' startdate2 <- "2009"
#' enddate2 <- "2013"
#' stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat"
#' CompareStats(stats,sites=sites,startDt=startdate,endDt=enddate,startDt2=startdate2,endDt2=enddate2)
CompareStats <- function(stats,sites="",dataPath="",startDt="",endDt="",sepChar=",",dataPath2="",startDt2="",endDt2="") {
  if (length(sites)>1) {
    statsout1 <- ObservedStatsUSGS(sites,startDt,endDt,stats)
    dataOut1 <- getDataUSGS(sites,startDt,endDt)
  } else {
    statsout1 <- ObservedStatsOtherMulti(dataPath,stats,startDt,endDt,sepChar)
    dataOut1 <- getDataLocal(dataPath,startDt,endDt,sepChar)
  }
  if (length(sites)>1) {
    if (nchar(dataPath)+nchar(dataPath2)<3) {
    statsout2 <- ObservedStatsUSGS(sites,startDt2,endDt2,stats)
    dataOut2 <- getDataUSGS(sites,startDt2,endDt2)
    } else if (nchar(dataPath)>1) {
    statsout2 <- ObservedStatsOtherMulti(dataPath,stats,startDt,endDt,sepChar)
    dataOut2 <- getDataLocal(dataPath,startDt,endDt,sepChar)
    } else if (nchar(dataPath2)>1) {
    statsout2 <- ObservedStatsOtherMulti(dataPath2,stats,startDt2,endDt2,sepChar)
    dataOut2 <- getDataLocal(dataPath2,startDt2,endDt2,sepChar)
    }
  } else if (nchar(dataPath2)>1) {
  statsout2 <- ObservedStatsOtherMulti(dataPath2,stats,startDt2,endDt2,sepChar)
  dataOut2 <- getDataLocal(dataPath2,startDt2,endDt2,sepChar)
  } else { statsout2 <- ObservedStatsOtherMulti(dataPath,stats,startDt2,endDt2,sepChar)
           dataOut2 <- getDataLocal(dataPath,startDt2,endDt2,sepChar)}  
DiffStats <- (statsout2[,4:190]-statsout1[,4:190])/statsout1[,4:190]
RegGoFstats <- RegionalGoF(statsout1[,c(1,4:190)],statsout2[,c(1,4:190)])
GoFstats <- matrix(,nrow=nrow(statsout1),ncol=146)
flag <- 0
for (i in 1:nrow(statsout1)) {
  a <- SiteGoF(dataOut1[[i]],dataOut2[[i]]) 
  if (length(a)>1) {
  GoFstats[i,] <- SiteGoF(dataOut1[[i]],dataOut2[[i]])
  } else {
    flag <- flag+1
    GoFstats[i,] <- rep(NA,146)
  }
}
if (flag==i) {
  compareOut <- list(statsout1,statsout2,DiffStats,RegGoFstats) 
} else {
colnames(GoFstats) <- c("nse","nselog","rmse","rmsne","rsr","pbias","pearson","spearman",'nse_90','nse_75_90','nse_50_75','nse_25_50','nse_10_25',
                  'nse_10','rmse_90','rmse_75_90','rmse_50_75','rmse_25_50','rmse_10_25','rmse_10','rmsne_90','rmsne_75_90','rmsne_50_75',
                  'rmsne_25_50','rmsne_10_25','rmsne_10','rsr_90','rsr_75_90','rsr_50_75','rsr_25_50','rsr_10_25','rsr_10','pbias_90',
                  'pbias_75_90','pbias_50_75','pbias_25_50','pbias_10_25','pbias_10','pearson_90','pearson_75_90','pearson_50_75',
                  'pearson_25_50','pearson_10_25','pearson_10','spearman_90','spearman_75_90','spearman_50_75','spearman_25_50',
                  'spearman_10_25','spearman_10','NSEbyMonthJan','NSELOGbyMonthJan','RMSEbyMonthJan','RMSNEbyMonthJan','RSRbyMonthJan',
                  'BiasbyMonthJan','PearsonbyMonthJan','SpearmanbyMonthJan','NSEbyMonthFeb','NSELOGbyMonthFeb','RMSEbyMonthFeb',
                  'RMSNEbyMonthFeb','RSRbyMonthFeb','BiasbyMonthFeb','PearsonbyMonthFeb','SpearmanbyMonthFeb','NSEbyMonthMar',
                  'NSELOGbyMonthMar','RMSEbyMonthMar','RMSNEbyMonthMar','RSRbyMonthMar','BiasbyMonthMar','PearsonbyMonthMar','SpearmanbyMonthMar',
                  'NSEbyMonthApr','NSELOGbyMonthApr','RMSEbyMonthApr','RMSNEbyMonthApr','RSRbyMonthApr','BiasbyMonthApr','PearsonbyMonthApr',
                  'SpearmanbyMonthApr','NSEbyMonthMay','NSELOGbyMonthMay','RMSEbyMonthMay','RMSNEbyMonthMay','RSRbyMonthMay','BiasbyMonthMay',
                  'PearsonbyMonthMay','SpearmanbyMonthMay','NSEbyMonthJun','NSELOGbyMonthJun','RMSEbyMonthJun','RMSNEbyMonthJun',
                  'RSRbyMonthJun','BiasbyMonthJun','PearsonbyMonthJun','SpearmanbyMonthJun','NSEbyMonthJul','NSELOGbyMonthJul',
                  'RMSEbyMonthJul','RMSNEbyMonthJul','RSRbyMonthJul','BiasbyMonthJul','PearsonbyMonthJul','SpearmanbyMonthJul','NSEbyMonthAug',
                  'NSELOGbyMonthAug','RMSEbyMonthAug','RMSNEbyMonthAug','RSRbyMonthAug','BiasbyMonthAug','PearsonbyMonthAug','SpearmanbyMonthAug',
                  'NSEbyMonthSep','NSELOGbyMonthSep','RMSEbyMonthSep','RMSNEbyMonthSep','RSRbyMonthSep','BiasbyMonthSep','PearsonbyMonthSep','SpearmanbyMonthSep',
                  'NSEbyMonthOct','NSELOGbyMonthOct','RMSEbyMonthOct','RMSNEbyMonthOct','RSRbyMonthOct','BiasbyMonthOct','PearsonbyMonthOct','SpearmanbyMonthOct',
                  'NSEbyMonthNov','NSELOGbyMonthNov','RMSEbyMonthNov','RMSNEbyMonthNov','RSRbyMonthNov','BiasbyMonthNov','PearsonbyMonthNov','SpearmanbyMonthNov',
                  'NSEbyMonthDec','NSELOGbyMonthDec','RMSEbyMonthDec','RMSNEbyMonthDec','RSRbyMonthDec','BiasbyMonthDec','PearsonbyMonthDec','SpearmanbyMonthDec')
compareOut <- list(statsout1,statsout2,DiffStats,RegGoFstats,GoFstats)
}
return(compareOut)
}