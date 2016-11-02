#' Indices describing duration of high flow events.
#' @description Calculates 24 indices used to describe the duration of high flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param stats A character vector containing names of indices to calculate or "All" for all stats. Specific statistics and groups of statistics are listed in details.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param drainArea A numeric specifying the drainage area. Only required for mh20 statistic. See details.
#' @param pref A character indicating whether to use mean or median in monthly aggregation. Only required for ma1_12 statistics. See details.
#' @details Descriptions of indices.
#' \itemize{
#' \item dh1 Annual maximum daily flow. Compute the maximum of a 1-day moving average flow for each year. DH1 is the 
#' mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' \item 
#' \item 
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' durationHigh(x=x)
#' 
durationHigh <- function(x,yearType = "water",digits=3,drainArea = NULL,pref="mean") {

}




