#' Function to decompose a flow series into flow direction change events 
#' 
#' @details This function accepts a vector of flow values, tests if each value in the flow vector
#' is greater or less than the preceding value of flow, and classifies the flow vector into events. An event
#' is defined as when a change in flow from one day to the next changes direction, e.g. rising limb to falling limb. 
#' If there is no change in flow from one day to the next it is considered part of the preceding event.
#' 
#' @param x A vector of flow values, should be sorted chronologically.
#' @return A dataframe with columns "flow" and "event"
#' @useDynLib EflowStats
#' @importFrom Rcpp sourceCpp
#' @export
#' @examples
#' x <- sampleData$discharge
#' calcChangeEvents(x)
calcChangeEvents <- function(x) {
        
        diffDays <- diff(x, lag = 1, 
                         differences = 1)
        
        changeDir <- ifelse(diffDays <0, -1, #fall
                            ifelse(diffDays ==0, NA, 1)) #rise
        
        #fill NAs with previous event number
        #This has to be done twice for the front and back ends
        #because the first NAs will not be carried forward and hte last NAs will not be caried backwarsd
        changeDir_forward <- zoo::na.locf(changeDir,na.rm=F)
        
        changeDir_backward <- zoo::na.locf(changeDir, fromLast=T,na.rm=F)
        
        changeDir <- changeDir_forward
        changeDir[is.na(changeDir)] <- changeDir_backward[is.na(changeDir)]

        runLengths <- rle2(changeDir)
        
        runLengths <- data.frame(lengths = runLengths$lengths,
                                 values = runLengths$values,
                                 eventNum = NA)
        
        #Make sequence of numbers to number events
        events <- seq(1:length(na.omit(runLengths$values)))
        
        #Number events
        runLengths$eventNum[!is.na(runLengths$values)] <- events
        eventVector <- rep(runLengths$eventNum,runLengths$lengths)
        eventVector <- c(NA,eventVector)
        
        changeEvents <- data.frame(flow=x,
                                   event=eventVector)
        
        return(changeEvents)
}

        