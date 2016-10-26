#' Indices describing magnitude of the peak flow condition.
#' @description Calculates 27 indices used to describe the magnitude of the peak flow condition. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param stats A character vector containing names of indices to calculate or "All" for all stats. Specific statistics and groups of statistics are listed in details.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param drainArea A numeric specifying the drainage area. Only required for mh20 statistic. See details.
#' @param pref A character indicating whether to use mean or median in monthly aggregation. Only required for ma1_12 statistics. See details.
#' @param asList Logial If TRUE returns a named list of statistics. If FALSE, returns a dataframe. Default is FALSE
#' @details Descriptions of indices.
#' \itemize{
#' \item mh1_12  Requires pref argument to be either "mean" or "median" specifying monthly aggregation function. 
#' Default is "mean". Means (or medians - use preference option) of maximum daily flow values for each month. 
#' For example, mh1 is the mean of all January maximum flow values over the entire record. 
#' \item mh13 variability (coefficient of variation) across maximum monthly flow values. Compute the mean 
#' and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation 
#' times 100 divided by the mean maximum monthly flow for all years (percent-spatial).
#' \item mh14 median of annual maximum flows. Compute the annual maximum flows from monthly maximum flows. 
#' Compute the ratio of annual maximum flow to median annual flow for each year. MH14 is the median of these ratios 
#' (dimensionless-temporal).
#' \item mh15_17 MH15; High flow discharge index. Compute the 1-percent exceedence value for the entire data record. MH15 is 
#' the 1-percent exceedence value divided by the median flow for the entire record (dimensionless-spatial). 
#' MH16; Compute the 10-percent exceedence value for the entire data record. MH16 is the 10-percent exceedence 
#' value divided by the median flow for the entire record (dimensionless-spatial). 
#' MH17; Compute the 25-percent exceedence value for the entire data record. MH17 is the 25-percent exceedence 
#' value divided by the median flow for the entire record (dimensionless-spatial). 
#' \item mh18 variability across annual maximum flows. Compute the logs (log10) of the maximum annual flows. 
#' Find the standard deviation and mean for these values. MH18 is the standard deviation times 100 divided by the 
#' mean (percent-spatial).
#' \item mh19 the skewness in annual maximum flows (dimensionless-spatial). Use the equation:
#' MH19   numerator =   N2 ? sum(qm3)-3N ? sum(qm) ? sum(qm2) + 2 ? (sum(qm))3  
#' denominator = N ? (N-1) ? (N-2) ? stddev3  
#' Where:  N = Number of years
#' qm = Log10 (annual maximum flows)
#' stddev = Standard deviation of the annual maximum flows
#' \item mh20 specific mean annual maximum flow. MH20 is the mean (or median-Use Preference option) of 
#' the annual maximum flows divided by the drainage area (cubic feet per second/square mile-temporal).
#' \item mh21_27 high flow volume indices. Compute the average volume for flow events above a threshold.
#' Thresholds are equal to the median flow for the entire record for mh21, 3 times the median flow for the 
#' entire record for mh22,and 7 times the median flow for the entire record for mh23. 
#' Thresholds are equal to the median flow for the entire record for mh24, 3 times the median flow for the 
#' entire record for mh25, 7 times the median flow for the entire record for mh26, 
#' and the 75th percentile for the entire record for mh27. 
#' MH21 through 23 are the average volumes divided by the median flow for the entire 
#' record (days-temporal). MH24 through 27 are the average peak flows divided by the median flow for the 
#' entire record (dimensionless-temporal)
#' }
#' @return A vector of selected flow statistics
#' @importFrom lubridate year
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' magHigh(x=x,stats="All")

magHigh <- function(x,stats = "All",yearType = "water",digits=3,drainArea = NULL,pref="mean",asList=FALSE) {
        
        ###Check dataframe inputs
        if(class(x[,1]) != "Date" && class(x[,2]) != "numeric")
        {
                stop("First column of x must contain a vector of class date.\nSecond column of x must contain a vector of class numeric.") 
        } else if (class(x[,1]) != "Date")
        {
                stop("First column of x must contain a vector of class date.") 
        } else if (class(x[,2]) != "numeric" & class(x[,2]) != "integer")
        {
                stop("Second column of x must contain a vector of class numeric.") 
        }
        if(!(yearType %in% c("water","calendar")))
        {
                stop("yearType must be one of either 'water' or 'calendar'")
        }
        
        ###rename dataframe for convenient use inside function
        names(x) <- c("date","flow")
        
        ###Get water year value
        if(yearType == "water")
        {
                x$year_val <- waterYear(x$date)
        } else {
                x$year_val <- lubridate::year(x$date)   
        }
        
        x$month_val <- lubridate::month(x$date)
        
        statsFuns <- list(mh1_12=mh1.12,
                          mh13=mh13,
                          mh14=mh14,
                          mh15_17 = mh15.17,
                          mh18=mh18,
                          mh19=mh19,
                          mh20=mh20,
                          mh21_27=mh21.27)
        if(stats == "All")
        {
                stats <- names(statsFuns)
        }
        
        statsOut <- lapply(statsFuns[stats], do.call, list(x=x,
                                                           drainArea=drainArea))
        
        if(asList == F)
        {
                statsOut <- unlist(statsOut)
                statsOut <- data.frame(indice = names(statsOut),
                                       value = unname(statsOut),
                                       stringsAsFactors = F)
                statsOut$value <- round(as.numeric(statsOut$value),digits=digits)
        }
        
        return(statsOut)
        
}




