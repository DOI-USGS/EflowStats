#' Indices describing rate of change of flow.
#' @description Calculates 9 indices used to describe the rate of change of flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param ... Optional arguments needed for \code{hitAllStats} function
#' @details Descriptions of indices.
#' \itemize{
#' \item ra1; Rise rate. Compute the change in flow for days in which the change is positive for the entire flow record. 
#' RA1 is the mean (or median-Use Preference option) of these values.
#' \item ra2; Variability in rise rate. Compute the standard deviation for the positive flow changes. RA2 is 100 times 
#' the standard deviation divided by the mean.
#' \item ra3; Fall rate. Compute the change in flow for days in which the change is negative for the entire flow record. 
#' RA3 is the mean (or median-Use Preference option) of these values.
#' \item ra4; Variability in fall rate. Compute the standard deviation for the negative flow changes. RA4 is 100 times 
#' the standard deviation divided by the mean.
#' \item ra5; Number of day rises. Compute the number of days in which the flow is greater than the previous day. RA5 
#' is the number of positive gain days divided by the total number of days in the flow record.
#' \item ra6; Change of flow. Compute the log of the flows for the entire flow record. Compute the change in log of flow 
#' for days in which the change is positive for the entire flow record. RA6 is the median of these values.
#' \item ra7; Change of flow. Compute the log of the flows for the entire flow record. Compute the change in log of 
#' flow for days in which the change is negative for the entire flow record. RA7 is the median of these log 
#' values.
#' \item ra8; Number of reversals. Compute the number of days in each year when the change in flow from one day to the 
#' next changes direction. RA8 is the average (or median - Use Preference option) of the yearly values.
#' \item ra9; Variability in reversals. Compute the standard deviation for the yearly reversal values. RA9 is 100 times the 
#' standard deviation divided by the mean.
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom stats median sd
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' yearType = "water"
#' hitRateChange(x=x,yearType=yearType)
hitRateChange <- function(x,yearType = "water",digits=3,pref="mean",...) {
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        
        #Calculate differences between day at i+1 and day at i for use in rate stats
        diffDays <- diff(x$discharge, lag = 1, 
                         differences = 1)
        riseValues <- diffDays[diffDays >0]
        fallValues <- diffDays[diffDays <0]
        
        #ra1.2
        if (pref == "mean") {
                ra1 <- mean(riseValues)
        } else {
                ra1 <- median(riseValues)
        }
        
        ra2 <- sd(riseValues)/mean(riseValues)*100
        
        #ra3.4 #This is slightly different than EflowStats due to rounding errors
        if (pref == "mean") {
                ra3 <- mean(fallValues)
        } else {
                ra3 <- median(fallValues)
        }
        
        ra4 <- sd(fallValues)/mean(fallValues)*100
        
        #ra5
        ra5 <- length(riseValues)/nrow(x)
        
        #ra6.7
        if(any(x$discharge ==0)) {
                warning("Discharge values of 0 were found, 0 values replace with 0.01 for RA6 and RA7 calculations")
        }
        
        logQ <- ifelse(x$discharge>0,log(x$discharge),log(.01))
        diffLogQ <- diff(logQ, lag = 1, 
                         differences = 1)
        riseLogQ <- diffLogQ[diffLogQ >0]
        fallLogQ <- diffLogQ[diffLogQ <0]
        
        ra6 <- median(abs(riseLogQ))
        ra7 <- median(fallLogQ)
        
        #ra8.9
        
        #calculate number of events per year
        yearlyEvents <- dplyr::do(dplyr::group_by(x,year_val),
                                  {
                                          calcChangeEvents(.$discharge)
                                  })
        numYearlyEvents <- dplyr::summarize(dplyr::group_by(yearlyEvents,year_val),
                                            numEvents = max(event,na.rm=T))
        
        ra8 <- mean(numYearlyEvents$numEvents)
        ra9 <- sd(numYearlyEvents$numEvents)/mean(numYearlyEvents$numEvents)*100
        
        #Return stats
        raOut <- data.frame(indice = c(paste0("ra",1:9)),
                            statistic = c(ra1,
                                          ra2,
                                          ra3,
                                          ra4,
                                          ra5,
                                          ra6,
                                          ra7,
                                          ra8,
                                          ra9),
                            stringsAsFactors = F
        )
        
        raOut$statistic <- round(raOut$statistic,digits=digits)
        
        return(raOut)
        
}


