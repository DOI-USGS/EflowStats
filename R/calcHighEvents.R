#' Function to decompose a flow series into high flow events defined as flows above a given threshold.
#' 
#' This function accepts a vector of flow values, tests if each value in the flow vector
#' is above or below a defined threshold, and classifies the flow vector into events. An event
#' is defined as consecutive entries above or below the given threshold.
#' 
#' @param flow A vector of flow values
#' @param threshold Threshold value defining event
#' @param type character indicating type of event. High flags events above threshold, low flags events below threshold.
#' @return A dataframe with columns "flow" and "event"
#' @export
#' @examples
#' x <- sampleData$discharge
#' threshold <- median(x,na.rm=TRUE)
#' calcHighEvents(x,threshold)
calcEvents <- function(x,threshold,type="high") {
        
        x <- data.frame(flow = x)
        
        if(type=="high")
        {
        #Calculate flows in excess of threshold
        #And define high flow as positive number

        x$event <- ifelse(x$flow > threshold,T,F)
        } else {
                x$event <- ifelse(x$flow < threshold,T,F)
                ###Remove first "event" since the time at strt of year before flood is not between 2 floods
        }
        
                
        #Calculate run lengths of T of F values to classify events
        runLengths <- rle(x$event)
        runLengths <- data.frame(lengths = runLengths$lengths,
                                 values = runLengths$values,
                                 eventNum = NA)
        
        #Make sequence of numbers to number events
        events <- seq(1:length(runLengths$values[runLengths$values==T]))
        
        #Number events
        runLengths$eventNum[runLengths$values==T] <- events
        eventVector <- rep(runLengths$eventNum,runLengths$lengths)
        
        #Bind events to X
        x$event <- eventVector
        
        flowEvents <- x[c("flow","event")]
        
        if(type=="low")
        {
        ###Remove first and last "event" since the time at strt of year before flood is not between 2 floods
                #flowEvents$event[flowEvents$event==1] <- NA
                flowEvents$event[flowEvents$event==max(flowEvents$event,na.rm=TRUE)] <- NA
                
        }
        
        return(flowEvents)
}
