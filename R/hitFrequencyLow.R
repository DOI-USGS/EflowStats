#' Indices describing frequency of low flow events.
#' @description Calculates 3 indices used to describe the frequency of low flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param ... Optional arguments needed for \code{hitAllStats} function
#' @details Descriptions of indices.
#' \itemize{
#' \item FL1; Low flood pulse count. Compute the average number of flow events with flows below a threshold equal to the 
#' 25th percentile value for the entire flow record. FL1 is the average (or median-Use Preference option) number of 
#' events.
#' \item FL2; Variability in low pulse count. Compute the standard deviation in the annual pulse counts for FL1. FL2 is 
#' 100 times the standard deviation divided by the mean pulse count.  
#' \item FL3; Frequency of low pulse spells. Compute the average number of flow events with flows below a threshold 
#' equal to 5 percent of the mean flow value for the entire flow record. FL3 is the average (or median-Use 
#' Preference option) number of events.
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom stats median na.omit quantile sd
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' yearType = "water"
#' hitFrequencyLow(x=x,yearType=yearType)
hitFrequencyLow <- function(x,yearType = "water",digits=3,pref="mean",...) {
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        
        medFlow <- median(x$discharge)
        
        #fl1.2
        #Pick out events for each year
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = quantile(x$discharge,probs=0.25,type=6),
                                                      type="low")
                                   }
        )
        
        #Replace NAs with 0
        yearlyCounts$event[is.na(yearlyCounts$event)] <- 0

        #Get number of events each year
        yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                         numEvents = max(event))
        
        if(pref=="mean") {
                fl1 <- mean(yearlyCounts$numEvents)
        } else {
                fl1 <- median(yearlyCounts$numEvents)
        }
        
        fl2 <- sd(yearlyCounts$numEvents)/mean(yearlyCounts$numEvents)*100
        
        #fl3
        #Pick out events for each year
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = 0.05*mean(x$discharge),
                                                      type="low")
                                   }
        )
        yearlyCounts <- na.omit(yearlyCounts)
        
        if(nrow(yearlyCounts) > 0)
        {
                #Get number of events each year
                yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                                 numEvents = max(event))
                if(pref=="mean") {
                        fl3 <- mean(yearlyCounts$numEvents)
                } else {
                        fl3 <- median(yearlyCounts$numEvents)
                }
        } else{fl3 <- 0}
        
        #Output stats
        flOut <- data.frame(indice = c(paste0("fl",1:3)),
                            statistic = c(fl1,
                                          fl2,
                                          fl3),
                            stringsAsFactors = F
        )
        
        flOut$statistic <- round(flOut$statistic,digits=digits)
        
        
        return(flOut)
}
