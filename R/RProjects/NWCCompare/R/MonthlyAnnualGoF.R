#' Function to calculate monthly annual GoF statistics for given observed and modeled data sets
#' 
#' This function accepts observed and modeled data frames of daily flow data and returns a data frame of 
#' calculated monthly annual GoF statistics
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
#' MonthlyAnnualGoF(Gaged,Modeled)
MonthlyAnnualGoF <- function(Gaged,Modeled) {
  NSEv <- vector(length=14)
  NSELOGv <- vector(length=length(NSEv))
  RMSEv <- vector(length=length(NSEv))
  PBIASv <- vector(length=length(NSEv))
  PEARSONv <- vector(length=length(NSEv))
  SPEARMANv <- vector(length=length(NSEv))
  
  i <- 1
  c <- 2
  GagedTmp <- aggregate(Gaged$discharge, list(Gaged$year_val), FUN = mean, na.rm=TRUE)
  ModeledTmp <- aggregate(Modeled$discharge, list(Modeled$year_val), FUN = mean, na.rm=TRUE)
  NSEv[i] <- nse(GagedTmp[,c],ModeledTmp[,c])
  NSELOGv[i] <- nselog(GagedTmp[,c],ModeledTmp[,c])
  RMSEv[i] <- rmse(GagedTmp[,c],ModeledTmp[,c])
  PBIASv[i] <- pbias(GagedTmp[,c],ModeledTmp[,c])
   PEARSONv[i] <- cor(GagedTmp[,c],ModeledTmp[,c],method="pearson")
  SPEARMANv[i] <- cor(GagedTmp[,c],ModeledTmp[,c],method="spearman")
  
  i <- 2
  c <- 3
  GagedTmp <- aggregate(Gaged$discharge, list(Gaged$year_val,Gaged$month_val), FUN = mean, na.rm=TRUE)
  ModeledTmp <- aggregate(Modeled$discharge, list(Modeled$year_val,Modeled$month_val), FUN = mean, na.rm=TRUE)
  NSEv[i] <- nse(GagedTmp[,c],ModeledTmp[,c])
  NSELOGv[i] <- nselog(GagedTmp[,c],ModeledTmp[,c])
  RMSEv[i] <- rmse(GagedTmp[,c],ModeledTmp[,c])
  PBIASv[i] <- pbias(GagedTmp[,c],ModeledTmp[,c])
  PEARSONv[i] <- cor(GagedTmp[,c],ModeledTmp[,c],method="pearson")
  SPEARMANv[i] <- cor(GagedTmp[,c],ModeledTmp[,c],method="spearman")
  
  c <- 2
  for (m in 1:12) {
    if (m<10) {month <- paste("0",m,sep="")
    } else {month<-paste("",m,sep="")}
    monthobs<-subset(Gaged,ifelse(nchar(month_val)<2,paste("0",month_val,sep=""),month_val)==month)
    monthmod<-subset(Modeled,ifelse(nchar(month_val)<2,paste("0",month_val,sep=""),month_val)==month)
    GagedTmp <- aggregate(monthobs$discharge, list(monthobs$year_val), FUN = mean, na.rm=TRUE)
    ModeledTmp <- aggregate(monthmod$discharge, list(monthmod$year_val), FUN = mean, na.rm=TRUE)
    i <- 2+m
    NSEv[i] <- nse(GagedTmp[,c],ModeledTmp[,c])
    NSELOGv[i] <- nselog(GagedTmp[,c],ModeledTmp[,c])
    RMSEv[i] <- rmse(GagedTmp[,c],ModeledTmp[,c])
    PBIASv[i] <- pbias(GagedTmp[,c],ModeledTmp[,c])
    PEARSONv[i] <- cor(GagedTmp[,c],ModeledTmp[,c],method="pearson")
    SPEARMANv[i] <- cor(GagedTmp[,c],ModeledTmp[,c],method="spearman")
  }
  
  Output <- c(NSEv,NSELOGv,RMSEv,PBIASv,PEARSONv,SPEARMANv)
  return(Output)
}