#' Function to return all flow statistics and comparison statistics for a set of USGS sites and start and end date
#' 
#' This function accepts a list of sites, one or two data paths, one or two sets of start and end dates and list of desired statistic groups and returns a data frame containing requested statistics
#' 
#' @param sites optional list of USGS station ids
#' @param dataPath path to local directory containing data files if \code{sites} unspecified.
#' @param stats string containing desired groups of statistics
#' @param startDt beginning water year, will be transalted to 10/01
#' @param endDt ending water year, will be translated to 09/30
#' @param sepChar string containing the datafile separator, default is comma
#' @param dataPath2 path to local directory containing data files if \code{sites} unspecified.
#' @param startDt2 beginning water year, will be transalted to 10/01
#' @param endDt2 ending water year, will be translated to 09/30
#' @return statsout List of data.frames containing requested statistics for each station. See details for description of data.frames. 
#' @details \code{CompareStats} outputs a list containing the following data.frames of requested statistics:
#' \describe{
#'      \item{statsoutsim}{data.frame of statistics for simulated data}
#'      \item{statsoutobs}{data.frame of statistics for observed data}
#'      \item{DiffStats}{data.frame of relative difference between simulated and observed statistics}
#'      \item{RegGoFstats}{data.frame of containing regional goodness of fit statistics between simulated and observed statistics.}
#'      \item{GoFstats}{data.frame of containing goodness of fit statistics between simulated and observed statistics.}

#' }
#' @export
#' @import dplyr
#' @examples
#' stats <- "magnifSeven,magStat,flowStat,durStat,timStat,rateStat"
#' sites <- c("02177000", "02178400")
#' startDt <- "2003"
#' endDt <- "2008"
#' startDt2 <- "2009"
#' endDt2 <- "2013"

#' \dontrun{
#' compareResults <- CompareStats(stats,sites=sites,
#'                                startDt=startdate,
#'                                endDt=enddate,
#'                                startDt2=startdate2,
#'                                endDt2=enddate2)
#' }                               
CompareStats <- function(stats,
                         sites="",
                         dataPath="",
                         startDt="",
                         endDt="",
                         sepChar=",",
                         dataPath2="",
                         startDt2="",
                         endDt2="") {
        
        if (length(sites)>1|nchar(sites[1])>1) { # If more than one USGS site, run ObservedStatsUSGS on them and get the data.
                statsoutsim <- ObservedStatsUSGS(sites,startDt,endDt,stats)
                dataOutsim <- getDataUSGS(sites,startDt,endDt)
        } else { # Otherwise just try and run statistics on the data at path 1.
                statsoutsim <- ObservedStatsOtherMulti(dataPath,stats,startDt,endDt,sepChar)
                dataOutsim <- getDataLocal(dataPath,startDt,endDt,sepChar)
        }
        
        if (length(sites)>1|nchar(sites[1])>1) { # if more than one USGS site is given get the second set of stats for usgs sites, or the stats for dataPath1 or dataPath2 
                if (nchar(dataPath)+nchar(dataPath2)<3) { # If the two data paths are empty get stats and data for the same sites but different time ranges.
                        statsoutobs <- ObservedStatsUSGS(sites,startDt2,endDt2,stats)
                        dataOutobs <- getDataUSGS(sites,startDt2,endDt2)
                } else if (nchar(dataPath)>1) { # If data path 1 is a thing get stats and data for the data at that path.
                        statsoutobs <- ObservedStatsOtherMulti(dataPath,stats,startDt,endDt,sepChar)
                        dataOutobs <- getDataLocal(dataPath,startDt,endDt,sepChar)
                } else if (nchar(dataPath2)>1) { # If data path 2 is a thing get stats and data for the data at that path. This will override dataPath1 numbers.
                        statsoutobs <- ObservedStatsOtherMulti(dataPath2,stats,startDt2,endDt2,sepChar)
                        dataOutobs <- getDataLocal(dataPath2,startDt2,endDt2,sepChar)
                }
        } else if (nchar(dataPath2)>1) { # If no sites given, calculate stats and get data for data at dataPath2
                statsoutobs <- ObservedStatsOtherMulti(dataPath2,stats,startDt2,endDt2,sepChar)
                dataOutobs <- getDataLocal(dataPath2,startDt2,endDt2,sepChar)
        } else { # calculate second set of stats on original local data with second date range.
                statsoutobs <- ObservedStatsOtherMulti(dataPath,stats,startDt2,endDt2,sepChar)
                dataOutobs <- getDataLocal(dataPath,startDt2,endDt2,sepChar)
        }  
        
        n <- ncol(statsoutsim)-1  
        
        statsoutsim <- statsoutsim[order(statsoutsim$site_no),]
        statsoutobs <- statsoutobs[order(statsoutobs$site_no),]
        
        ###Check that ordering is consitant between dataframes
        if(!all.equal(statsoutsim$site_no,statsoutobs$site_no))
        {
                stop("Inconsistant sites in simulated and observed data")
        }
        
        statsoutsim2 <- statsoutsim[which(is.na(statsoutsim$comment) | nchar(statsoutsim$comment)==0),]
        statsoutobs2 <- statsoutobs[which(is.na(statsoutobs$comment) | nchar(statsoutobs$comment)==0),]
        
        statsoutsim2 <- statsoutsim2[statsoutsim2$site_no %in% statsoutobs2$site_no,]
        statsoutobs2 <- statsoutobs2[statsoutobs2$site_no %in% statsoutsim2$site_no,]
        
        DiffStats <- (statsoutsim2[,4:n]-statsoutobs2[,4:n])/statsoutobs2[,4:n]
        diffnames <- colnames(DiffStats)
        DiffStats <- cbind(statsoutsim2$site_no,DiffStats,stringsAsFactors=FALSE)
        diffnames <- c("site_no",diffnames)
        colnames(DiffStats) <- diffnames
        
        sitesnum <- min(length(dataOutsim),length(dataOutobs))
        
        RegGoFstats <- RegionalGoF(statsoutobs2[,c(1,4:n)],statsoutsim2[,c(1,4:n)])
        GoFstats <- matrix(nrow=sitesnum,ncol=147)
        flag <- 0

        modeledFlow <- dplyr::bind_rows(dataOutsim)
        gagedFlow <- dplyr::bind_rows(dataOutobs)
        
        ###Get vector of unique sites to loop over. Loop should be removed in future because
        ###This can be vectorized, but will put off for now.
        
        sites <- unique(c(modeledFlow$site_no,gagedFlow$site_no))
        
        

        for (i in 1:sitesnum) {
                
                siteModeledFlow <- modeledFlow[modeledFlow$site_no == sites[i],]
                siteGagedFlow <- gagedFlow[gagedFlow$site_no == sites[i],]
                
                ###Check there is modeled flow
                if(nrow(siteModeledFlow) == 0) 
                {
                        warning(paste("Site number",sites[i],"missing modeled flow"))
                        next
                }
                
                ###Check there is gaged flow
                if(nrow(siteGagedFlow) == 0) 
                {
                        warning(paste("Site number",sites[i],"missing gaged flow"))
                        next
                }
                
                ###Run siteGOF
                a <- SiteGoF(siteModeledFlow,
                             siteGagedFlow) 
                
                GoFnames <- colnames(a)
                
                if (length(a)>1) {
                        GoFstats[i,] <- t(as.vector(SiteGoF(flow2,flow1)))
                } else {
                        flag <- flag+1
                        GoFstats[i,] <- rep(NA,147)
                }
        }
        
        if (flag==i) {
                compareOut <- list(statsoutsim = statsoutsim,
                                   statsoutobs = statsoutobs,
                                   DiffStats = DiffStats,
                                   RegGoFstats = RegGoFstats) 
        } else {
                GoFstats <- as.data.frame(GoFstats,stringsAsFactors=FALSE)
                colnames(GoFstats) <- GoFnames
                compareOut <- list(statsoutsim = statsoutsim,
                                   statsoutobs = statsoutobs,
                                   DiffStats = DiffStats,
                                   RegGoFstats = RegGoFstats,
                                   GoFstats = GoFstats)
                
        }
        return(compareOut)
}