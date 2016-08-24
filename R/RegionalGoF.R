#' Function to calculate GoF statistics for given observed and modeled statistics
#' 
#' This function accepts data frames of statistics for observed and modeled daily flow time-series 
#' and returns a data frame of calculated GoF statistics
#' 
#' @param simStats data.frame of statistics output by \code{ObservedStatsUSGS},
#'  \code{ObservedStatsOther}, or \code{ObservedStatsOtherMulti} to be considered simulated data for comparison purposes.
#' @param simStats data.frame of statistics output by \code{ObservedStatsUSGS},
#'  \code{ObservedStatsOther}, or \code{ObservedStatsOtherMulti} to be considered observed data for comparison purposes.
#' @return Output data frame of calculated statistics
#' @export
RegionalGoF <- function(simStats,obsStats) {
        
        simStats <- reshape2::melt(simStats,
                                   id.vars=c("site_no","min_date","max_date"))
        
        
  Output<-matrix(nrow=nrow(GagedFlowStats)*6,ncol=ncol(GagedFlowStats)+1)
  for (j in 1:nrow(GagedFlowStats)) {
    n<-(j-1)*6
    Output[1+n,2] <- GagedFlowStats[j,1]
    Output[2+n,2] <- GagedFlowStats[j,1]
    Output[3+n,2] <- GagedFlowStats[j,1]
    Output[4+n,2] <- GagedFlowStats[j,1]
    Output[5+n,2] <- GagedFlowStats[j,1]
    Output[6+n,2] <- GagedFlowStats[j,1]
    Output[1+n,1] <- "nse"
    Output[2+n,1] <- "nselog"
    Output[3+n,1] <- "rmse"
    Output[4+n,1] <- "pbias"
    Output[5+n,1] <- "pearson"
    Output[6+n,1] <- "spearman"
    for (i in 3:ncol(GagedFlowStats)) {
      Output[1+n,i] <- nse(GagedFlowStats[,i],ModeledFlowStats[,i])
      Output[2+n,i] <- nselog(GagedFlowStats[,i],ModeledFlowStats[,i])
      Output[3+n,i] <- rmse(GagedFlowStats[,i],ModeledFlowStats[,i])
      Output[4+n,i] <- pbias(ModeledFlowStats[,i],GagedFlowStats[,i])
      Output[5+n,i] <- cor(GagedFlowStats[,i],ModeledFlowStats[,i],method='pearson')
      Output[6+n,i] <- cor(GagedFlowStats[,i],ModeledFlowStats[,i],method='spearman')
    }
  }
  Output <- as.data.frame(Output,stringsAsFactors=FALSE)
  a <- colnames(GagedFlowStats)
  a <- c("stat",a)
  colnames(Output) <- (a)
  Output <- Output[order(as.numeric(Output$site_no)),]
  
  
  
  return(Output)
}