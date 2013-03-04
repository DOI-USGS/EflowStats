#' Function to calculate GoF statistics for given observed and modeled data sets
#' 
#' This function accepts observed and modeled data frames of daily flow data and returns a data frame of 
#' calculated GoF statistics
#' 
#' @param Gaged data frame of daily flow data
#' @param Modeled data frame of daily flow data
#' @return Output data frame of calculated statistics
#' @export
#' @examples
#' load_data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
#' load_mod<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
#' Gaged<-read.csv(load_data,stringsAsFactors=FALSE)
#' Modeled<-read.csv(load_mod,stringsAsFactors=FALSE)
#' SiteGoF(Gaged,Modeled)
SiteGoF <- function(Gaged,Modeled) {
  nsev<-nse(Gaged$discharge,Modeled$discharge)
  nselogv<-nselog(Gaged$discharge,Modeled$discharge)
  rmsev<-rmse(Gaged$discharge,Modeled$discharge)
  pbiasv<-pbias(Modeled$discharge,Gaged$discharge)
  pearsonv<-cor(Modeled$discharge,Gaged$discharge,method="pearson")
  spearmanv<-cor(Modeled$discharge,Gaged$discharge,method="spearman")
  
  obs_percentiles <- quantile(Gaged$discharge,probs=c(0.10, 0.25, 0.50, 0.75, 0.90),na.rm=TRUE)
  obs_10_indices <- which(Gaged$discharge< obs_percentiles[1])
  obs_10_25_indices <- which(Gaged$discharge>obs_percentiles[1] 
                             & Gaged$discharge<obs_percentiles[2])
  obs_25_50_indices <- which(Gaged$discharge>obs_percentiles[2]
                             & Gaged$discharge<obs_percentiles[3])
  obs_50_75_indices <- which(Gaged$discharge>obs_percentiles[3]  
                             & Gaged$discharge<obs_percentiles[4])
  obs_75_90_indices <- which(Gaged$discharge>obs_percentiles[4]  
                             & Gaged$discharge<obs_percentiles[5])
  obs_90_indices <- which(Gaged$discharge>obs_percentiles[5])
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
  pbiasv_90 <- pbias(Gaged$discharge[obs_90_indices],Modeled$discharge[obs_90_indices])
  pbiasv_75_90 <- pbias(Gaged$discharge[obs_75_90_indices],Modeled$discharge[obs_75_90_indices])
  pbiasv_50_75 <- pbias(Gaged$discharge[obs_50_75_indices],Modeled$discharge[obs_50_75_indices])
  pbiasv_25_50 <- pbias(Gaged$discharge[obs_25_50_indices],Modeled$discharge[obs_25_50_indices])
  pbiasv_10_25 <- pbias(Gaged$discharge[obs_10_25_indices],Modeled$discharge[obs_10_25_indices])
  pbiasv_10 <- pbias(Gaged$discharge[obs_10_indices],Modeled$discharge[obs_10_indices])
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
  BiasbyMonth <-vector(length=12)
  PearsonbyMonth <-vector(length=12)
  SpearmanbyMonth <-vector(length=12)
  
  for (m in 1:12) {
    if (m<10) {month <- paste("0",m,sep="")
    } else {month<-paste("",m,sep="")}
    monthobs<-subset(Gaged,month_val==month)
    monthmod<-subset(Modeled,month_val==month)
    NSEbyMonth[m] <- nse(monthobs$discharge,monthmod$discharge)
    NSELOGbyMonth[m] <- nselog(monthobs$discharge,monthmod$discharge)
    RMSEbyMonth[m] <- rmse(monthobs$discharge,monthmod$discharge)
    BiasbyMonth[m] <- pbias(monthobs$discharge,monthmod$discharge)
    PearsonbyMonth[m] <- cor(monthobs$discharge,monthmod$discharge,method="pearson")
    SpearmanbyMonth[m] <- cor(monthobs$discharge,monthmod$discharge,method="spearman")
  }
  
  Output <- c(nsev,nselogv,rmsev,pbiasv,pearsonv,spearmanv,
              nsev_90,nsev_75_90,nsev_50_75,nsev_25_50,nsev_10_25,nsev_10,
              rmsev_90,rmsev_75_90,rmsev_50_75,rmsev_25_50,rmsev_10_25,rmsev_10,
              pbiasv_90,pbiasv_75_90,pbiasv_50_75,pbiasv_25_50,pbiasv_10_25,pbiasv_10,           
              pearsonv_90,pearsonv_75_90,pearsonv_50_75,pearsonv_25_50,pearsonv_10_25,pearsonv_10,
              spearmanv_90,spearmanv_75_90,spearmanv_50_75,spearmanv_25_50,spearmanv_10_25,spearmanv_10,
              NSEbyMonth,NSELOGbyMonth,RMSEbyMonth,BiasbyMonth,PearsonbyMonth,SpearmanbyMonth)
  return(Output)
}