#' Function to calculate GoF statistics for given observed and modeled statistics
#' 
#' This function accepts data frames of statistics for observed and modeled daily flow time-series 
#' and returns a data frame of calculated GoF statistics
#' 
#' @param GagedFlowStats data frame of flow stats for observed data
#' @param ModeledFlowStats data frame of flow stats for modeled data
#' @return Output data frame of calculated statistics
#' @export
RegionalGoF <- function(GagedFlowStats,ModeledFlowStats) {
  Output<-matrix(nrow=6,ncol=ncol(GagedFlowStats))
  for (i in 1:ncol(GagedFlowStats)) {
    Output[1,i] <- nse(GagedFlowStats[,i],ModeledFlowStats[,i])
    Output[2,i] <- nselog(GagedFlowStats[,i],ModeledFlowStats[,i])
    Output[3,i] <- rmse(GagedFlowStats[,i],ModeledFlowStats[,i])
    Output[4,i] <- pbias(ModeledFlowStats[,i],GagedFlowStats[,i])
    Output[5,i] <- cor(GagedFlowStats[,i],ModeledFlowStats[,i],method='pearson')
    Output[6,i] <- cor(GagedFlowStats[,i],ModeledFlowStats[,i],method='spearman')
  }
  return(Output)
}