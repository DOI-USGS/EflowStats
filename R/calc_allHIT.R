#' Function to calculate all 171 biologically relevant hydrologic indice statistics described in the USGS Hydrologic Index Tool
#' 
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param wyMonth The month of the year in which the water year starts.
#' @param digits A numeric. Number of digits to round indice values
#' @param stats Character vector statistic groups. Choices are "all","calc_magAverage", "calc_magLow", "calc_magHigh", "calc_frequencyLow", "calc_frequencyHigh", "calc_durationLow", "calc_durationHigh", "calc_timingAverage", "calc_timingLow", "calc_timingHigh", "calc_rateChange"     
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param drainArea Numeric drainage area. Only required for some statistics. Typically units of square miles, see details of specific statistics for more. 
#' @param floodThreshold Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
#' @details This function is a wrapper for the lower level functions to calculate groups of indice statistics.
#' Desired groups of indices can be specified using the \code{stats} argument. 
#' Different arguments are required depending on indices selected and are described below.
#' Detailed descriptions of the indices and \code{pref} argument behavior are provided in the documentation for individual indice group functions
#' \itemize{
#' \item calc_magAverage Indices describing magnitude of the average flow condition. \code{drainArea} argument must be specified for the ma41 statistic. 
#' \item calc_magLow Indices describing magnitude of the low flow condition. \code{drainArea} argument must be specified for the ml22 statistic. 
#' \item calc_magHigh Indices describing magnitude of the peak flow condition. \code{drainAre} argument must be specified for the mh20 statistic.
#' \item calc_frequencyLow Indices describing frequency of low flow events. No additional arguments required.
#' \item calc_frequencyHigh Indices describing frequency of high flow events. No additional arguments required.
#' \item calc_durationLow Indices describing duration of low flow events. No additional arguments required.
#' \item calc_durationHigh Indices describing duration of high flow events. floodThreshold needs to be supplied. See \code{\link{calc_durationHigh}}.
#' \item calc_timingAverage Indices describing timing of average flow events. No additional arguments required.
#' \item calc_timingLow Indices describing timing of low flow events. No additional arguments required.
#' \item calc_timingHigh Indices describing timing of high flow events. No additional arguments required.
#' \item calc_rateChange Indices describing rate of change of flow. No additional arguments required.
#' }
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' calc_allHIT(x=x,yearType="water",stats="all",pref="mean",drainArea=50)
calc_allHIT <- function(x,yearType="water",wyMonth=10L,stats="all",digits=3,pref="mean",drainArea=NULL,floodThreshold = NULL) {
        

        statsFuns <- list(
                calc_magAverage=calc_magAverage,
                calc_magLow=calc_magLow,
                calc_magHigh=calc_magHigh,
                calc_frequencyLow=calc_frequencyLow,
                calc_frequencyHigh=calc_frequencyHigh,
                calc_durationLow=calc_durationLow,
                calc_durationHigh=calc_durationHigh,
                calc_timingAverage=calc_timingAverage,
                calc_timingLow=calc_timingLow,
                calc_timingHigh=calc_timingHigh,
                calc_rateChange=calc_rateChange)
        
        if("all" %in% stats)
        {
                stats <- c("calc_magAverage",
                           "calc_magLow",
                           "calc_magHigh",
                           "calc_frequencyLow",
                           "calc_frequencyHigh",
                           "calc_durationLow",
                           "calc_durationHigh",
                           "calc_timingAverage",
                           "calc_timingLow",
                           "calc_timingHigh",
                           "calc_rateChange")
        }
        
        statsOut <- lapply(statsFuns[stats], do.call, list(x=x,
                                                           yearType=yearType,
                                                           wyMonth=wyMonth,
                                                           digits=digits,
                                                           pref=pref,
                                                           drainArea=drainArea,
                                                           floodThreshold=floodThreshold))

        statsOut <- dplyr::bind_rows(statsOut)
        
        return(statsOut)
}

        