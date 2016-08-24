#' Function to return all flow statistics and comparison statistics for a set of USGS sites and start and end date
#' 
#' This function accepts a list of sites, one or two data paths, one or two sets of start and end dates and list of desired statistic groups and returns a data frame containing requested statistics
#' 
#' @param simStats data.frame of statistics output by \code{ObservedStatsUSGS},
#'  \code{ObservedStatsOther}, or \code{ObservedStatsOtherMulti} to be considered simulated data for comparison purposes.
#' @param simStats data.frame of statistics output by \code{ObservedStatsUSGS},
#'  \code{ObservedStatsOther}, or \code{ObservedStatsOtherMulti} to be considered observed data for comparison purposes.
#' @details \code{CompareStats} outputs a list containing the following data.frames of requested statistics:
#' \describe{
#'      \item{simStats}{data.frame of statistics for simulated data}
#'      \item{obsStats}{data.frame of statistics for observed data}
#'      \item{DiffStats}{data.frame of relative difference in between simulated and observed statistics}
#'      \item{RegGoFstats}{data.frame of containing regional goodness of fit statistics between simulated and observed statistics.}
#'      \item{GoFstats}{data.frame of containing goodness of fit statistics between simulated and observed statistics.}

#' }
#' @export
#' @import dplyr
#' @importFrom reshape2 melt
#' @examples


#' \dontrun{
#' compareResults <- CompareStats(stats,sites=sites,
#'                                startDt=startdate,
#'                                endDt=enddate,
#'                                startDt2=startdate2,
#'                                endDt2=enddate2)
#' }                               
compareStats <- function(simStats,
                         obsStats) {
        
        ###Remove any sites with comment flags
        simStats <- simStats[which(is.na(simStats$comment)),]
        obsStats <- obsStats[which(is.na(obsStats$comment)),]
        
        ###Check for duplicated site IDs. Can only have unique site ideas because dataframes matched on site ID
        if(any(duplicated(simStats$site_no) == TRUE))
        {
                stop("simStats contains duplicated site numbers. Only one set of statistics allowed per site")
        }
        
        if(any(duplicated(obsStats$site_no) == TRUE))
        {
                stop("obsStats contains duplicated site numbers. Only one set of statistics allowed per site")
        }
        
        
        simStats <- simStats[order(simStats$site_no),]
        obsStats <- obsStats[order(obsStats$site_no),]
        
        ###Check that ordering is consitant between dataframes
        if(!all.equal(simStats$site_no,obsStats$site_no))
        {
                stop("Inconsistant site numbers between simulated and observed data")
        }
        
        if(!(all(colnames(simStats)==colnames(obsStats))==TRUE))
        {
                stop("Incosistant statistics in simulated and observed data")
        }
        
        
        numCols <- ncol(simStats)
        
        DiffStats <- (simStats[,4:numCols]-obsStats[,4:numCols])/obsStats[,4:numCols]
        diffnames <- colnames(DiffStats)
        DiffStats <- cbind(simStats$site_no,DiffStats,stringsAsFactors=FALSE)
        diffnames <- c("site_no",diffnames)
        colnames(DiffStats) <- diffnames
        
        #sitesnum <- min(length(dataOutsim),length(dataOutobs))
        
        RegGoFstats <- RegionalGoF(obsStats[,c(1,4:numCols)],simStats[,c(1,4:numCols)])
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
                compareOut <- list(simStats = simStats,
                                   obsStats = obsStats,
                                   DiffStats = DiffStats,
                                   RegGoFstats = RegGoFstats) 
        } else {
                GoFstats <- as.data.frame(GoFstats,stringsAsFactors=FALSE)
                colnames(GoFstats) <- GoFnames
                compareOut <- list(simStats = simStats,
                                   obsStats = obsStats,
                                   DiffStats = DiffStats,
                                   RegGoFstats = RegGoFstats,
                                   GoFstats = GoFstats)
                
        }
        return(compareOut)
}