#' Function to decompose a flow series into high flow events defined as flows above a given threshold.
#' 
#' This function accepts a vector of flow values, tests if each value in the flow vector
#' is above a defined threshold, classifies the flow vector into events, and returns the 
#' average event duration. An event
#' is defined as consecutive entries above the given threshold. 
#' 
#' @param flow A vector of numeric flow values
#' @param threshold Numeric threshold value for high flow events
#' @param average Logical. If TRUE returns only the average event duration. If FALSE returns the duration for each individual event 
#' @return A numeric of the mean event duration in units of number of rows if average = TRUE. Otherwise, returns a datafarme of durations for each event.
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData$discharge
#' threshold <- median(x,na.rm=TRUE)
#' eventDuration(x,threshold)
eventDuration <- function(x,threshold,average = TRUE,type="high",pref="mean") {
        
        flowEvents <- calcEvents(x=x,threshold=threshold,type=type)
        flowEvents <- na.omit(flowEvents)

        eventDurations <- dplyr::summarize(dplyr::group_by(flowEvents,event),
                                           duration = length(event)
        )
        
        if(average == TRUE)
        {
                if(pref=="mean")
                {
                eventDurations <- mean(eventDurations$duration)
                } else if (pref=="median")
                {
                        eventDurations <- median(eventDurations$duration)
                }
        }
        
        return(eventDurations)
}