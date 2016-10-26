#' Function to decompose a discharge series into high flow events defined as flows above a given threshold.
#' 
#' This function accepts a vector of flow values, tests if each value in the flow vector
#' is above a defined threshold, and classifies the discharge vector into events. An event
#' is defined as consecutive entries above the given threshold.
#' 
#' @param flow A vector of flow values
#' @return A dataframe with columns "discharge" and "event"
#' @examples
#' x <- sampleData$discharge
#' threshold <- median(x,na.rm=TRUE)
#' highFlowEvents(x,threshold)
calcHighEvents <- function(x,threshold) {
        
        x <- data.frame(discharge = x)
        
        #Calculate flows in excess of threshold
        #And define high flow as positive number
        x$highFlow <- ifelse(x$discharge - threshold > 0,T,F)
        
        #Calculate run lengths of T of F values to classify events
        runLengths <- rle(x$highFlow)
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
        
        flowEvents <- x[c("discharge","event")]
        return(flowEvents)
}
