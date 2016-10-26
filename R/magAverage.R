#' Indices describing magnitude of the average flow condition.
#' @description Calculates 45 indices used to describe the magnitude of the average flow condition. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param stats A character vector containing names of indices to calculate or "All" for all stats. Specific statistics and groups of statistics are listed in details.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param drainArea A numeric specifying the drainage area. Only required for ma41 statistic. See details.
#' @param pref A character indicating whether to use mean or median in monthly aggregation. Only required for ma12_23 statistics. See details.
#' @param asList Logial If TRUE returns a named list of statistics. If FALSE, returns a dataframe. Default is FALSE
#' @details Descriptions of indices.
#' \itemize{
#' \item ma1  Mean of the daily mean flow values for the entire flow record 
#' \item ma2  Median of the daily mean flow values for the entire flow record 
#' \item  ma3  Mean (or median - use preference option) of the coefficients of 
#' variation (standard deviation/mean) for each year. Compute the coefficient 
#' of variation for each year of daily flows. Compute the mean of the annual 
#' coefficients of variation 
#' \item ma4  Standard deviation of the percentiles of the logs of the entire flow record divided by the mean of percentiles of the logs. Compute the log(10) of the daily flows for the entire record. Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 75th, 80th, 85th, 90th and 95th percentiles for the logs of the entire flow record. Percentiles are computed by interpolating between the ordered (ascending) logs of the flow values. Compute the standard deviation and mean for the percentile values. Divide the standard deviation by the mean 
#' \item ma5  The skewness of the entire flow record is computed as the mean for the entire flow record (ma1) divided by the median (ma2) for the entire flow record 
#' \item ma6  Range in daily flows is the ratio of the 10-percent to 90-percent exceedence values for the entire flow record. Compute the 5-percent to 95-percent exceedence values for the entire flow record. Exceedence is computed by interpolating between the ordered (descending) flow values. Divide the 10-percent exceedence by the 90-percent value 
#' \item ma7  Range in daily flows is computed in the same way as ma6 except using the 20-percent and 80-percent exceedence values. Divide the 20-percent exceedence value by the 80-percent value 
#' \item ma8  Range in daily flows is computed in the same way as ma6 except using the 25-percent and 75-percent exceedence values. Divide the 25-percent exceedence value by the 75-percent value 
#' \item ma9  Spread in daily flows is the ratio of the difference between the 90th and 10th percentile of the logs of the flow data to the log of the median of the entire flow record. Compute the log(10) of the daily flows for the entire record. Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 75th, 80th, 85th, 90th and 95th percentiles for the logs of the entire flow record. Percentiles are computed by interpolating between the ordered (ascending) logs of the flow values. Compute ma9 as (90th-10th)/log10(ma2) 
#' \item ma10  Spread in daily flows is computed in the same way as ma9 except using the 20th and 80th percentiles 
#' \item ma11  Spread in daily flows is computed in the same way as ma9 except using the 25th and 75th percentiles. 
#' \item ma12_23  Requires pref argument to be either "mean" or "median" specifying monthly aggregation function. Default is "mean". Means (or medians - use preference option) of monthly flow values. Compute the means for each month over the entire flow record. For example, ma12 is the mean of all January flow values over the entire record. 
#' \item ma24_35  Variability (coefficient of variation) of monthly flow values. Compute the standard deviation for each month in each year over the entire flow record. Divide the standard deviation by the mean for each month. Take the mean (or median - use preference option) of these values for each month across all years. 
#' \item ma36_40  Variability and skewness across monthly flows. ma36 - compute the minimum, maximum and mean flows for each month in the entire flow record. ma36 is the maximum monthly flow minus the minimum monthly flow divided by the median monthly flow. ma37 - compute the first (25th percentile) and the third (75th percentile) quartiles. ma37 is the third quartile minus the first quartile divided by the median of the monthly means. ma38 - compute the 10th and 90th percentiles for the monthly means. ma38 is the 90th percentile minus the 10th percentile divided by the median of the monthly means. ma39 - compute the standard deviation for the monthly means. ma39 is the standard deviation times 100 divided by the mean of the monthly means. ma40 - skewness in the monthly flows. ma40 is the mean of the monthly flow means minus the median of the monthly means divided by the median of the monthly means.
#' \item ma41_45 ma41 requires drainArea to be specified. Annual runoff and the variability and skewness across annual flows. ma41 - compute the annual mean daily flows. ma41 is the mean of the annual means divided by the drainage area. ma42 is the maximum annual flow minus the minimum annual flow divided by the median annual flow. ma43 - compute the first (25th percentile) and third (75th percentile) quartiles for the annual means. ma43 is the third quartile minus the first quartile divided by the median of the annual means. ma44 - compute the 10th and 90th percentiles for the annual means. ma44 is the 90th percentile minus the 10th percentile divided by the median of the annual means. ma45 - skewness in the annual flows. ma45 is the mean of the annual flow means minus the median of the annual means divided by the median of the annual means.
#' }
#' @return A vector of selected flow statistics
#' @importFrom lubridate year
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' magAverage(x=x,stats="All")

magAverage <- function(x,stats = "All",yearType = "water",digits=3,drainArea = NULL,pref="mean",asList = FALSE) {
        
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
        
        ##Calculations needed for MA4 through MA11
        percentiles <- quantile(x$flow,probs=seq(0.05,0.95,0.05),type=6)
        percMean <- mean(percentiles,na.rm=TRUE)
        percSD <- sd(percentiles,na.rm=TRUE)
        
        statsFuns <- list(ma1=ma1,
                          ma2=ma2,
                          ma3=ma3,
                          ma4=ma4,
                          ma5=ma5,
                          ma6=ma6,
                          ma7=ma7,
                          ma8=ma8,
                          ma9=ma9,
                          ma10=ma10,
                          ma11=ma11,
                          ma12_23=ma12.23,
                          ma24_35=ma24.35,
                          ma36_40=ma36.40,
                          ma41_45=ma41.45)
        if(stats == "All")
        {
                stats <- names(statsFuns)
        }
        
        statsOut <- lapply(statsFuns[stats], do.call, list(x=x,
                                                           percentiles=percentiles,
                                                           percMean = percMean,
                                                           percSD = percSD,
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






