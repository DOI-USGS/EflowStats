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
#' @import plyr
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
    statsoutsim <- ObservedStatsUSGS(sites,startDt,endDt,stats)
    dataOutsim <- getDataUSGS(sites,startDt,endDt)
  } else {
    statsoutsim <- ObservedStatsOtherMulti(dataPath,stats,startDt,endDt,sepChar)
    dataOutsim <- getDataLocal(dataPath,startDt,endDt,sepChar)
  }
  if (length(sites)>1) {
    if (nchar(dataPath)+nchar(dataPath2)<3) {
    statsoutobs <- ObservedStatsUSGS(sites,startDt2,endDt2,stats)
    dataOutobs <- getDataUSGS(sites,startDt2,endDt2)
    } else if (nchar(dataPath)>1) {
    statsoutobs <- ObservedStatsOtherMulti(dataPath,stats,startDt,endDt,sepChar)
    dataOutobs <- getDataLocal(dataPath,startDt,endDt,sepChar)
    } else if (nchar(dataPath2)>1) {
    statsoutobs <- ObservedStatsOtherMulti(dataPath2,stats,startDt2,endDt2,sepChar)
    dataOutobs <- getDataLocal(dataPath2,startDt2,endDt2,sepChar)
    }
  } else if (nchar(dataPath2)>1) {
  statsoutobs <- ObservedStatsOtherMulti(dataPath2,stats,startDt2,endDt2,sepChar)
  dataOutobs <- getDataLocal(dataPath2,startDt2,endDt2,sepChar)
  } else { statsoutobs <- ObservedStatsOtherMulti(dataPath,stats,startDt2,endDt2,sepChar)
           dataOutobs <- getDataLocal(dataPath,startDt2,endDt2,sepChar)}  
n <- ncol(statsoutsim)-1  
statsoutsim <- statsoutsim[order(as.numeric(statsoutsim$site_no)),]
statsoutobs <- statsoutobs[order(as.numeric(statsoutobs$site_no)),]
statsoutsim2 <- statsoutsim[which(is.na(statsoutsim$comment) | nchar(statsoutsim$comment)==0),]
statsoutobs2 <- statsoutobs[which(is.na(statsoutobs$comment) | nchar(statsoutobs$comment)==0),]
statsoutsim2 <- statsoutsim2[as.numeric(statsoutsim2$site_no) %in% as.numeric(statsoutobs2$site_no),]
statsoutobs2 <- statsoutobs2[as.numeric(statsoutobs2$site_no) %in% as.numeric(statsoutsim2$site_no),]
DiffStats <- (statsoutsim2[,4:n]-statsoutobs2[,4:n])/statsoutobs2[,4:n]
diffnames <- colnames(DiffStats)
DiffStats <- cbind(statsoutsim2$site_no,DiffStats,stringsAsFactors=FALSE)
diffnames <- c("site_no",diffnames)
colnames(DiffStats) <- diffnames
sitesnum <- min(length(dataOutsim),length(dataOutobs))
RegGoFstats <- RegionalGoF(statsoutsim2[,c(1,4:n)],statsoutobs2[,c(1,4:n)])
GoFstats <- matrix(,nrow=sitesnum,ncol=147)
flag <- 0
for (i in 1:sitesnum) {
  flow1 <- dataOutsim[[i]] 
  lfunc <- function(e) {data.frame(e$site_no,e$wy_val,e$date,e$discharge,e$month_val,e$year_val,e$day_val,e$jul_val,stringsAsFactors=FALSE)}
  flow2 <- ldply(dataOutobs,lfunc)
  flow2 <- flow2[as.numeric(flow2$e.site_no)==max(as.numeric(flow1$site_no)),]
  colnames(flow2) <- colnames(flow1)
  a <- SiteGoF(flow1,flow2) 
  GoFnames <- colnames(a)
  if (length(a)>1) {
  GoFstats[i,] <- t(as.vector(SiteGoF(flow1,flow2)))
  } else {
    flag <- flag+1
    GoFstats[i,] <- rep(NA,147)
  }
}
if (flag==i) {
  compareOut <- list(statsoutsim,statsoutobs,DiffStats,RegGoFstats) 
} else {
GoFstats <- as.data.frame(GoFstats,stringsAsFactors=FALSE)
colnames(GoFstats) <- GoFnames
compareOut <- list(statsoutsim,statsoutobs,DiffStats,RegGoFstats,GoFstats)
}
return(compareOut)
}