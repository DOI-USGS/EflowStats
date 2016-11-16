#' Indices describing duration of low flow events.
#' @description Calculates 24 indices used to describe the duration of low flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param stats A character vector containing names of indices to calculate or "All" for all stats. Specific statistics and groups of statistics are listed in details.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param drainArea A numeric specifying the drainage area. Only required for mh20 statistic. See details.
#' @param pref A character indicating whether to use mean or median in monthly aggregation. Only required for ma1_12 statistics. See details.
#' @param lowflowThreshold Numeric low flow threshold as the flow equivalent for a flood recurrence of 1.67 years
#' @details Descriptions of indices.
#' \itemize{
#' \item DL1 Annual minimum daily flow. Compute the minimum 1-day average flow for each year. DL1 is the mean 
#' (or median-Use Preference option) of these values.
#' \item DL2 Annual minimum of 3-day moving average flow. Compute the minimum of a 3-day moving average flow for 
#' each year. DL2 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' \item DL3 Annual minimum of 7-day moving average flows. Compute the minimum of a 7-day moving average flow for 
#' each year. DL3 is the mean (or median-Use Preference option) of these values.
#' \item DL4 Annual minimum of 30-day moving average flows. Compute the minimum of a 30-day moving average flow for 
#' each year. DH4 is the mean (or median-Use Preference option) of these values.
#' \item DL5 Annual minimum of 90-day moving average flows. Compute the minimum of a 90-day moving average flow for 
#' each year. DH5 is the mean (or median-Use Preference option) of these values.
#' \item DL6 Variability of annual minimum daily average flow. Compute the standard deviation for the minimum daily 
#' average flow. DL6 is 100 times the standard deviation divided by the mean (percent-spatial).
#' \item DL7 Variability of annual minimum of 3-day moving average flows. Compute the standard deviation for the 
#' minimum 3-day moving averages. DL7 is 100 times the standard deviation divided by the mean.
#' \item DL8 Variability of annual minimum of 7-day moving average flows. Compute the standard deviation for the 
#' minimum 7-day moving averages. DL8 is 100 times the standard deviation divided by the mean.
#' \item DL9 Variability of annual minimum of 30-day moving average flows. Compute the standard deviation for the 
#' minimum 30-day moving averages. DL9 is 100 times the standard deviation divided by the mean.
#' \item DL10 Variability of annual minimum of 90-day moving average flows. Compute the standard deviation for the 
#' minimum 90-day moving averages. DH10 is 100 times the standard deviation divided by the mean.
#' \item DL11 Annual minimum daily flow divided by the median for the entire record. Compute the minimum daily flow 
#' for each year. DL11 is the mean of these values divided by the median for the entire record.
#' \item DL12 Annual minimum of 7-day moving average flows divided by the median for the entire record. Compute the 
#' minimum of a 7-day moving average flow for each year. DL12 is the mean of these values divided by the median 
#' for the entire record.
#' \item DL13 Annual minimum of 30-day moving average flows divided by the median for the entire record. Compute the 
#' minimum of a 30-day moving average flow for each year. DL13 is the mean of these values divided by the median 
#' for the entire record.
#' \item DL14 Low exceedence flows. Compute the 75-percent exceedence value for the entire flow record. DL14 is 
#' the exceedence value divided by the median for the entire record.
#' \item DL15 Low exceedence flows. Compute the 90-percent exceedence value for the entire flow record. DL15 is the 
#' exceedence value divided by the median for the entire record.
#' \item DL16 Low flow pulse duration. Compute the average pulse duration for each year for flow events below a 
#' threshold equal to the 25th percentile value for the entire flow record. DL16 is the median of the yearly 
#' average durations.
#' \item DL17; Variability in low pulse duration. Compute the standard deviation for the yearly average low pulse durations. 
#' DL17 is 100 times the standard deviation divided by the mean of the yearly average low pulse durations.
#' \item DH18 High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' three times the median flow value for the entire flow record. DH18 is the mean 
#' duration of the events.
#' \item DH19 High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' seven times the median flow value for the entire flow record. DH19 is the mean
#' duration of the events .
#' \item DH20 High flow duration. Compute the 75th percentile value for the entire flow record. Compute the average 
#' duration of flow events with flows above a threshold equal to the 75th percentile value for the median annual 
#' flows. DH20 is the average duration of the events.
#' \item DH21 High flow duration. Compute the 25th percentile value for the entire flow record. Compute the average 
#' duration of flow events with flows above a threshold equal to the 25th percentile value for the entire set 
#' of flows. DH21 is the average duration of the events. 
#' \item DH22 Flood interval. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Determine the median number of days between flood events for each year. DH22 is the mean (or median-Use 
#' Preference option) of the yearly median number of days between flood events.
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom RcppRoll roll_mean
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' durationHigh(x=x)
#' 
durationLow <- function(x,yearType = "water",digits=3,drainArea = NULL,pref="mean",lowflowThreshold = NULL) {
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
        
        if(any(is.na(x)))
        {
                stop("dataframe x cannot contain NA values")
        }
        
        ###rename dataframe for convenient use inside function
        names(x) <- c("date","discharge")
        
        ###Order by date
        x <- x[order(x$date),]
        ###Get water year value
        if(yearType == "water")
        {
                x$year_val <- waterYear(x$date)
        } else {
                x$year_val <- lubridate::year(x$date)   
        }
        
        
        x$month_val <- lubridate::month(x$date)
        
        #Calculate max and medians by month and year for statistics
        flowSum_year <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         minFlow = min(discharge),
                                         medFlow = median(discharge))
        flowSum_yearMon <- dplyr::summarize(dplyr::group_by(x,year_val,month_val),
                                            minFlow = min(discharge),
                                            medFlow = median(discharge),
                                            meanFlow = mean(discharge))
        minRollingMean <- dplyr::summarize(dplyr::group_by(x,year_val),
                                           minRoll3Mean = min(RcppRoll::roll_mean(discharge,n=3L)),
                                           minRoll7Mean = min(RcppRoll::roll_mean(discharge,n=7L)),
                                           minRoll30Mean = min(RcppRoll::roll_mean(discharge,n=30L)),
                                           minRoll90Mean = min(RcppRoll::roll_mean(discharge,n=90L))
        )
        medFlow <- median(x$discharge)
        
        #dl1
        if (pref == "mean") {
                dl1 <- mean(flowSum_year$minFlow)
        } else {
                dl1 <- median(flowSum_year$minFlow)
        }
        
        #dl2-dl5
        if (pref == "mean") {
                dl2.5 <- apply(minRollingMean[2:5],
                               mean,
                               MARGIN=2)
        } else {
                dl2.5 <- apply(minRollingMean[2:5],
                               median,
                               MARGIN=2)
        }
        ##Unname
        dl2.5 <- unname(dl2.5)
        
        #dh6-10
        dl6 <- (sd(flowSum_year$minFlow) * 100)/mean(flowSum_year$minFlow)
        dl7.10 <- apply(minRollingMean[2:5],
                        function(x) (sd(x)*100)/mean(x),
                        MARGIN=2)
        dl7.10 <- unname(dl7.10)
        
        #dh11-13
        dl11 <- mean(flowSum_year$minFlow)/medFlow
        dl12.13 <- dl2.5[c(2,3)]/medFlow 
        ##Unname
        dl12.13 <- unname(dl12.13)
        
        #dl14.15
        quant25.10 <- quantile(x$discharge,c(.25,.1),type=6)
        dl14.15 <- quant25.10/median(x$discharge)
        dl14.15 <- unname(dl14.15)
        
        #dl16.17 . 
        yearlyDurations <- dplyr::summarize(dplyr::group_by(x,year_val),
                                            avgDuration = eventDuration(x=discharge,
                                                                        threshold = quantile(discharge,probs=0.25,type=6),
                                                                        average = TRUE,
                                                                        type="low")
        )
        
        dl16 <- median(yearlyDurations$avgDuration)
        dl17 <- (sd(yearlyDurations$avgDuration)*100)/mean(yearlyDurations$avgDuration)
        
        
        

        
        
        
        
        