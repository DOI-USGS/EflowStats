#' Function to decompose a flow series into flow direction change events 
#' 
#' @details This function accepts a vector of flow values, tests if each value in the flow vector
#' is greater or less than the preceding value of flow, and classifies the flow vector into events. An event
#' is defined as when a change in flow from one day to the next changes direction, e.g. rising limb to falling limb. 
#' If there is no change in flow from one day to the next it is considered part of the preceding event.
#' If there is no change in flow for the whole timeseries, the timeseries is assigned event 0.
#' 
#' @param x A vector of flow values, should be sorted chronologically.
#' @return A dataframe with columns "flow" and "event"
#' @importFrom stats na.omit
#' @importFrom imputeTS na_locf
#' @export
#' @examples
#' x <- sampleData$discharge
#' find_changeEvents(x)
find_changeEvents <- function(x) {
        
        diffDays <- diff(x, lag = 1, 
                         differences = 1)
        
        changeDir <- sign(diffDays)
        changeDir[changeDir==0] <- NA
        
        if(all(is.na(changeDir))){
                #No changes in this timeseries
                changeEvents <- data.frame(flow = x,
                                           event = 0)
        }else{
                changeDir <- na_locf(changeDir, na_remaining="rev")
                
                runLengths <- rle(changeDir)
                
                runLengths <- data.frame(lengths = runLengths$lengths,
                                         values = runLengths$values,
                                         eventNum = NA)
                
                #Make sequence of numbers to number events
                events <- seq_along(na.omit(runLengths$values))
                
                #Number events
                runLengths$eventNum[!is.na(runLengths$values)] <- events
                eventVector <- rep.int(runLengths$eventNum,runLengths$lengths)
                
                #substract 1 event from event vector to make it start at 0 
                #since there is no knowledge of the flow before the start of the record
                eventVector <- eventVector - 1
                
                #Fill in first dropped entry with a 0 event to max length of flow
                eventVector <- c(0,eventVector)
                
                
                changeEvents <- data.frame(flow = x,
                                           event = eventVector)  
        }
        
        return(changeEvents)
}