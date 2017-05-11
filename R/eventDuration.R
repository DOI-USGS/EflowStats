#' Function to decompose a flow series into high flow events defined as flows above a given threshold.
#' 
#' This function accepts a vector of flow values, tests if each value in the flow vector
#' is above or below a defined threshold, classifies the flow vector into events, and returns the 
#' average event duration or all event durations if average = FALSE. An event
#' is defined as consecutive entries above the given threshold. 
#' 
#' @param x A vector of numeric flow values
#' @param threshold Numeric threshold value for high flow events
#' @param aggType Character vector indicating type of aggregation to use, if any. Choices are "average", "min", "max" or "none". If "average", mean or median of events is returned depending on "pref" setting If "none", returns the duration for each individual event 
#' @param type Character of either "high" or "low" indicating if events are defined as flows below or above the given threshold, respectively
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param trim Logical. Events that start or end at the beginning or end of the record are dropped if \code{TRUE} because an accurate duration can't be calculated if teh start or end time is unknown.
#' @return A numeric of the mean event duration in units of number of rows if average = TRUE. Otherwise, returns a datafarme of durations for each event.
#' @importFrom stats median na.omit
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData$discharge
#' threshold <- median(x,na.rm=TRUE)
#' eventDuration(x,threshold)
eventDuration <- function(x,threshold,aggType = "average",type="high",pref="mean",trim=FALSE) {
        
        flowEvents <- calcEvents(x=x,threshold=threshold,type=type)
        
        #Check if event is runs up until the beginning or end of hte period of record
        #If it does, remove the event because there is no start or end time to calculate duration
        
        if(trim == TRUE) {
                if(!is.na(flowEvents$event[1])) {
                        flowEvents$event[flowEvents$event == flowEvents$event[1]] <- NA
                }
                
                if(!is.na(flowEvents$event[length(flowEvents$event)])) {
                        flowEvents$event[flowEvents$event == flowEvents$event[length(flowEvents$event)]] <- NA
                }
        }
        
        flowEvents <- na.omit(flowEvents)
        
        eventDurations <- dplyr::summarize(dplyr::group_by(flowEvents,event),
                                           duration = length(event)
        )
        
        if(nrow(eventDurations) > 0)
        {
                if(aggType == "average")
                {
                        if(pref=="mean")
                        {
                                eventDurations <- mean(eventDurations$duration,na.rm=TRUE)
                        } else if (pref=="median")
                        {
                                eventDurations <- median(eventDurations$duration,na.rm=TRUE)
                        }
                }
                
                if(aggType == "min")
                {
                        eventDurations <- min(eventDurations$duration,na.rm=TRUE)
                }
                
                if(aggType == "max")
                {
                        eventDurations <- max(eventDurations$duration,na.rm=TRUE)
                }
                
        } else {eventDurations <- 0}
        
        return(eventDurations)
}