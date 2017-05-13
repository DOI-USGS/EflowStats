#' Indices describing frequency of high flow events.
#' @description Calculates 11 indices used to describe the frequency of high flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param floodThreshold Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
#' @param ... Optional arguments needed for \code{hitAllStats} function
#' @details Descriptions of indices.
#' \itemize{
#' \item fh1 High flood pulse count. Compute the average number of flow events with flows above a threshold equal to 
#' the 75th percentile value for the entire flow record. FH1 is the average (or median-Use Preference option) 
#' number of events.
#' \item fh2 Variability in high pulse count. Compute the standard deviation in the annual pulse counts for FH1. FH2 
#' is 100 times the standard deviation divided by the mean pulse count (number of events/year-spatial).  
#' \item fh3 High flood pulse count. Compute the average number of days per year that the flow is above a threshold equal 
#' to three times the median flow for the entire record. FH3 is the mean (or median-Use Preference option) of the 
#' annual number of days for all years.
#' \item fh4 High flood pulse count. Compute the average number of days per year that the flow is above a threshold 
#' equal to seven times the median flow for the entire record. FH4 is the mean (or median - Use Preference option) 
#' of the annual number of days for all years.
#' \item fh5 Flood frequency. Compute the average number of flow events with flows above a threshold equal to the 
#' median flow value for the entire flow record. FH5 is the average (or median - Use Preference option) number 
#' of events.
#' \item fh6 Flood frequency. Compute the average number of flow events with flows above a threshold equal to three 
#' times the median flow value for the entire flow record. FH6 is the average (or median-Use Preference option) 
#' number of events.
#' \item fh7 Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' seven times the median flow value for the entire flow record. FH7 is the average (or median-Use Preference option) 
#' number of events.
#' \item fh8 Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' 25-percent exceedence value for the entire flow record. FH8 is the average (or median-Use Preference option) 
#' number of events.
#' \item fh9 Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' 75-percent exceedence value for the entire flow record. FH9 is the average (or median-Use Preference option) 
#' number of events.
#' \item fh10 Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' median of the annual minima for the entire flow record. FH10 is the average (or median-Use Preference option) 
#' number of events.
#' \item fh11 Flood frequency. Compute the average number of flow events with flows above a threshold equal to flow 
#' corresponding to a 1.67-year recurrence interval. FH11 is the average (or median-Use Preference option) number 
#' of events.
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
#' floodThreshold = 1158
#' hitFrequencyHigh(x=x,yearType=yearType,floodThreshold = 1158)
hitFrequencyHigh <- function(x,yearType = "water",digits=3,pref="mean",floodThreshold = NULL,...) {
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        
        #Calculate min by year for statistics
        flowSum_year <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         minFlow = min(discharge))
        
        
        medFlow <- median(x$discharge)
        
        #fh1.2 #This differs from original EflowStats because of bug in fh1.2 where the qfiletempf dataframe was not ordered on date
        percentiles <- quantile(x$discharge,probs=0.75,type=6)
        #Pick out events for each year
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = percentiles["75%"],
                                                      type="high")
                                   }
        )
        
        #Replace NAs with 0
        yearlyCounts$event[is.na(yearlyCounts$event)] <- 0

        #Get number of events each year
        yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                         numEvents = max(event))
        
        if(pref=="mean") {
                fh1 <- mean(yearlyCounts$numEvents)
        } else {
                fh1 <- median(yearlyCounts$numEvents)
        }
        
        fh2 <- sd(yearlyCounts$numEvents)/mean(yearlyCounts$numEvents)*100
        
        #fh3
        x$highFlow <- ifelse(x$discharge>3*medFlow,TRUE,FALSE)
        highFlowCount <- dplyr::summarize(dplyr::group_by(x,year_val),
                                          numDays = length(highFlow[highFlow==TRUE]))
        
        if(pref=="mean") {
                fh3 <- mean(highFlowCount$numDays)
        } else {
                fh3 <- median(highFlowCount$numDays)
        }
        
        #fh4
        x$highFlow <- ifelse(x$discharge>7*medFlow,TRUE,FALSE)
        highFlowCount <- dplyr::summarize(dplyr::group_by(x,year_val),
                                          numDays = length(highFlow[highFlow==TRUE]))
        
        if(pref=="mean") {
                fh4 <- mean(highFlowCount$numDays)
        } else {
                fh4 <- median(highFlowCount$numDays)
        }
        
        #fh5
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = medFlow,
                                                      type="high")
                                   }
        )
        yearlyCounts <- na.omit(yearlyCounts)
        
        #Get number of events each year
        yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                         numEvents = max(event))
        
        if(pref=="mean") {
                fh5 <- mean(yearlyCounts$numEvents)
        } else {
                fh5 <- median(yearlyCounts$numEvents)
        }
        
        #fh6
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = 3*medFlow,
                                                      type="high")
                                   }
        )
        
        #Replace NAs with 0 so years with 0 events are counted
        yearlyCounts$event[is.na(yearlyCounts$event)] <- 0
        
        #Get number of events each year
        yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                         numEvents = max(event))
        
        if(pref=="mean") {
                fh6 <- mean(yearlyCounts$numEvents)
        } else {
                fh6 <- median(yearlyCounts$numEvents)
        }
        
        #fh7
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = 7*medFlow,
                                                      type="high")
                                   }
        )
        
        #Replace NAs with 0 so years with 0 events are counted
        yearlyCounts$event[is.na(yearlyCounts$event)] <- 0
        
        #Get number of events each year
        yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                         numEvents = max(event))
        
        if(pref=="mean") {
                fh7 <- mean(yearlyCounts$numEvents)
        } else {
                fh7 <- median(yearlyCounts$numEvents)
        }
        
        #fh8
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = quantile(x$discharge,.75,type=6),
                                                      type="high")
                                   }
        )
        
        #Replace NAs with 0 so years with 0 events are counted
        yearlyCounts$event[is.na(yearlyCounts$event)] <- 0
        
        #Get number of events each year
        yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                         numEvents = max(event))
        
        if(pref=="mean") {
                fh8 <- mean(yearlyCounts$numEvents)
        } else {
                fh8 <- median(yearlyCounts$numEvents)
        }
        
        #fh9
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = quantile(x$discharge,.25,type=6),
                                                      type="high")
                                   }
        )
        #Replace NAs with 0 so years with 0 events are counted
        yearlyCounts$event[is.na(yearlyCounts$event)] <- 0
        
        #Get number of events each year
        yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                         numEvents = max(event))
        
        if(pref=="mean") {
                fh9 <- mean(yearlyCounts$numEvents)
        } else {
                fh9 <- median(yearlyCounts$numEvents)
        }
        
        #fh10
        yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                   {
                                           calcEvents(.$discharge,
                                                      threshold = median(flowSum_year$minFlow),
                                                      type="high")
                                   }
        )
        yearlyCounts <- na.omit(yearlyCounts)
        
        #Get number of events each year
        yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                         numEvents = max(event))
        
        if(pref=="mean") {
                fh10 <- mean(yearlyCounts$numEvents)
        } else {
                fh10 <- median(yearlyCounts$numEvents)
        }
        
        #fh11
        if(!is.null(floodThreshold))
        {
                yearlyCounts <-  dplyr::do(dplyr::group_by(x,year_val),
                                           {
                                                   calcEvents(.$discharge,
                                                              threshold = floodThreshold,
                                                              type="high")
                                           }
                )
                yearlyCounts$event[is.na(yearlyCounts$event)] <- 0
                
                #Get number of events each year
                yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts,year_val),
                                                 numEvents = max(event))
                
                if(pref=="mean") {
                        fh11 <- mean(yearlyCounts$numEvents)
                } else {
                        fh11 <- median(yearlyCounts$numEvents)
                }
                
        } else(fh11 <- NA)
        
        #Output stats
        fhOut <- data.frame(indice = c(paste0("fh",1:11)),
                            statistic = c(fh1,
                                          fh2,
                                          fh3,
                                          fh4,
                                          fh5,
                                          fh6,
                                          fh7,
                                          fh8,
                                          fh9,
                                          fh10,
                                          fh11),
                            stringsAsFactors = F
        )
        
        fhOut$statistic <- round(fhOut$statistic,digits=digits)
        
        return(fhOut)
}

        
        
        
        
        