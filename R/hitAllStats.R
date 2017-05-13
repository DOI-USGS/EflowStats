#' hitAllStats
#' 
#' Function to calculate all 171 biologically relevant hydrologic indice statistics described in the USGS Hydrologic Index Tool
#' 
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param stats Character vector statistic groups. Choices are "all","hitMagAverage", "hitMagLow", "hitMagHigh", "hitFrequencyLow", "hitFrequencyHigh", "hitDurationLow", "hitDurationHigh", "hitTimingAverage", "hitTimingLow", "hitTimingHigh", "hitRateChange"     
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param drainArea Numeric drainage area. Only required for some statistics. Typically units of square miles, see details of specific statistics for more. 
#' @param floodThreshold Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
#' @details This function is a wrapper for the lower level functions to calculate groups of indice statistics.
#' Desired groups of indices can be specified using the \code{stats} argument. 
#' Different arguments are required depending on indices selected and are described below.
#' Detailed descriptions of the indices and \code{pref} argument behavior are provided in the documentation for individual indice group functions
#' \itemize{
#' \item hitMagAverage Indices describing magnitude of the average flow condition. \code{drainArea} argument must be specified for the ma41 statistic. 
#' \item hitMagLow Indices describing magnitude of the low flow condition. \code{drainArea} argument must be specified for the ml22 statistic. 
#' \item hitMagHigh Indices describing magnitude of the peak flow condition. \code{drainAre} argument must be specified for the mh20 statistic.
#' \item hitFrequencyLow Indices describing frequency of low flow events. No additional arguments required.
#' \item hitFrequencyHigh Indices describing frequency of high flow events. No additional arguments required.
#' \item hitDurationLow Indices describing duration of low flow events. No additional arguments required.
#' \item hitDurationHigh Indices describing duration of high flow events. floodThreshold needs to be supplied. See \code{\link{hitDurationHigh}}.
#' \item hitTimingAverage Indices describing timing of average flow events. No additional arguments required.
#' \item hitTimingLow Indices describing timing of low flow events. No additional arguments required.
#' \item hitTimingHigh Indices describing timing of high flow events. No additional arguments required.
#' \item hitRateChange Indices describing rate of change of flow. No additional arguments required.
#' }
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' hitAllStats(x=x,yearType="water",stats="all",pref="mean",drainArea=50)
hitAllStats <- function(x,yearType = "water",stats="all",digits=3,pref="mean",drainArea=NULL,floodThreshold = NULL) {
        

        statsFuns <- list(
                hitMagAverage=hitMagAverage,
                hitMagLow=hitMagLow,
                hitMagHigh=hitMagHigh,
                hitFrequencyLow=hitFrequencyLow,
                hitFrequencyHigh=hitFrequencyHigh,
                hitDurationLow=hitDurationLow,
                hitDurationHigh=hitDurationHigh,
                hitTimingAverage=hitTimingAverage,
                hitTimingLow=hitTimingLow,
                hitTimingHigh=hitTimingHigh,
                hitRateChange=hitRateChange)
        
        if("all" %in% stats)
        {
                stats <- c("hitMagAverage",
                           "hitMagLow",
                           "hitMagHigh",
                           "hitFrequencyLow",
                           "hitFrequencyHigh",
                           "hitDurationLow",
                           "hitDurationHigh",
                           "hitTimingAverage",
                           "hitTimingLow",
                           "hitTimingHigh",
                           "hitRateChange")
        }
        
        statsOut <- lapply(statsFuns[stats], do.call, list(x=x,
                                                           yearType=yearType,
                                                           digits=digits,
                                                           pref=pref,
                                                           drainArea=drainArea,
                                                           floodThreshold=floodThreshold))

        statsOut <- dplyr::bind_rows(statsOut)
        
        return(statsOut)
}

        