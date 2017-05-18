#' Indices describing magnitude of the average flow condition.
#' @description Calculates 45 indices used to describe the magnitude of the average flow condition. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param drainArea A numeric specifying the drainage area. Only required for ma41 statistic. Typically square miles, see details.
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param ... Optional arguments needed for \code{hitAllStats} function
#' @details Descriptions of indices.
#' \itemize{
#' \item ma1  Mean of the daily mean flow values for the entire flow record 
#' \item ma2  Median of the daily mean flow values for the entire flow record 
#' \item ma3  Mean (or median - use preference option) of the coefficients of 
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
#' @return A data.frame flow statistics
#' @import dplyr
#' @importFrom lubridate year
#' @importFrom stats median quantile sd
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' hitMagAverage(x=x,yearType=yearType,drainArea=drainArea)
#' 
hitMagAverage <- function(x,yearType = "water",digits=3,drainArea = NULL,pref="mean",...) {
        
        # Check pref input
        if(!pref %in% c("mean", "median")){ stop("Preference must be either mean or median") }
        
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        
        # percentiles
        percentiles <- quantile(x$discharge,probs=seq(0.05,0.95,0.05),type=6)
        percentiles_log10 <- quantile(log10(x$discharge),probs=seq(0.05,0.95,0.05),type=6)
        
        meanFlow <- mean.default(x$discharge)     
        medFlow <- percentiles["50%"]
        medFlow_log10 <- log10(medFlow)
        
        #ma1-2
        ma1 <- meanFlow
        ma2 <- medFlow
        
        #ma3
        yearAgg <- dplyr::summarize(dplyr::group_by(x,year_val),
                                    meanFlow = mean.default(discharge), #ma41-45
                                    CV = sd(discharge)/meanFlow) #ma3
        if(pref=="mean")
        {
                ma3 <- mean.default(yearAgg$CV)*100
        } else {
                ma3 <- median(yearAgg$CV)*100   
        }
        
        #ma4-8
        percMean <- mean.default(percentiles_log10)
        percSD <- sd(percentiles_log10)
        
        ma4 <- percSD/percMean*100
        ma5 <- meanFlow/medFlow
        ma6 <- as.numeric(percentiles["90%"]/percentiles["10%"])
        ma7 <- as.numeric(percentiles["80%"]/percentiles["20%"])
        ma8 <- as.numeric(percentiles["75%"]/percentiles["25%"])
        
        #ma9-11
        
        ma9 <- as.numeric(percentiles_log10["90%"]-percentiles_log10["10%"])/medFlow_log10
        ma10 <- as.numeric(percentiles_log10["80%"]-percentiles_log10["20%"])/medFlow_log10
        ma11 <- as.numeric(percentiles_log10["75%"]-percentiles_log10["25%"])/medFlow_log10
        
        #ma12-23
        if (pref== "mean") {
                flowSum_month <- dplyr::summarize(dplyr::group_by(x,month_val),
                                                  meanFlow = mean.default(discharge))
                ma12.23 <- flowSum_month$meanFlow
        } else {
                flowSum_month <- dplyr::summarize(dplyr::group_by(x,month_val),
                                                  medianFlow = median(discharge))
                ma12.23 <- flowSum_month$medianFlow
        }
        
        #ma24-35
        yearMonthAgg = dplyr::summarize(dplyr::group_by(x,year_val,month_val),
                                        meanFlow = mean.default(discharge), #ma36-40
                                        CV = sd(discharge)/meanFlow #), #ma24-35
                                        #minFlow = min(discharge), #ma36-40 #UNUSED?
                                        #maxFlow = max(discharge) #ma36-40 #UNUSED?
        )
        
        if(pref == "mean") {
                ma24.35 <- dplyr::summarize(dplyr::group_by(yearMonthAgg,month_val),
                                            meanCV = mean.default(CV))
                ma24.35 <- ma24.35$meanCV*100
        } else {
                ma24.35 <- dplyr::summarize(dplyr::group_by(yearMonthAgg,month_val),
                                            medianCV = median(CV))
                ma24.35 <- ma24.35$medianCV*100
        }
        
        #ma36-40
        medMonthlyFlow <- median(yearMonthAgg$meanFlow)
        meanMonthlyFlow <- mean.default(yearMonthAgg$meanFlow)
        
        percentiles <- quantile(yearMonthAgg$meanFlow,probs=c(0.1,0.25,0.75,0.9),type=6)
        
        ma36 <- (max(yearMonthAgg$meanFlow) -min(yearMonthAgg$meanFlow))/medMonthlyFlow
        
        ma37 <- (percentiles["75%"]-percentiles["25%"])/medMonthlyFlow
        ma38 <- (percentiles["90%"]-percentiles["10%"])/medMonthlyFlow
        ma39 <- (sd(yearMonthAgg$meanFlow)*100)/meanMonthlyFlow
        ma40 <- (meanMonthlyFlow-medMonthlyFlow)/medMonthlyFlow
        
        #ma41-45        
        medYearlyFlow <- median(yearAgg$meanFlow)
        meanYearlyFlow <- mean.default(yearAgg$meanFlow)
        
        percentiles <- quantile(yearAgg$meanFlow,probs=c(0.1,0.25,0.75,0.9),type=6)
        
        if(!is.null(drainArea))
        {
                ma41 <- meanYearlyFlow/drainArea     
        } else(ma41 <- NA)
        
        
        ma42 <- (max(yearAgg$meanFlow) -min(yearAgg$meanFlow))/medYearlyFlow
        ma43 <- (percentiles["75%"]-percentiles["25%"])/medYearlyFlow
        ma44 <- (percentiles["90%"]-percentiles["10%"])/medYearlyFlow
        ma45 <- (meanYearlyFlow-medYearlyFlow)/medYearlyFlow
        
        
        #Output the dataframe
        maOut <- data.frame(indice = c(paste0("ma",1:45)),
                            statistic = c(ma1,
                                          ma2,
                                          ma3,
                                          ma4,
                                          ma5,
                                          ma6,
                                          ma7,
                                          ma8,
                                          ma9,
                                          ma10,
                                          ma11,
                                          ma12.23,
                                          ma24.35,
                                          ma36,
                                          ma37,
                                          ma38,
                                          ma39,
                                          ma40,
                                          ma41,
                                          ma42,
                                          ma43,
                                          ma44,
                                          ma45),
                            stringsAsFactors = F
        )
        
        maOut$statistic <- round(maOut$statistic,digits=digits)
        
        
        return(maOut)
        
}


