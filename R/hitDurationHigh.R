#' Indices describing duration of high flow events.
#' @description Calculates 24 indices used to describe the duration of high flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param floodThreshold Numeric value of flood threshold as the flow equivalent for a flood recurrence of 1.67 years. Can be calculated using the \code{peakThreshold} function.
#' @param ... Optional arguments needed for \code{hitAllStats} function
#' @details Descriptions of indices.
#' \itemize{
#' \item dh1 Annual maximum daily flow. Compute the maximum of a 1-day moving average flow for each year. dh1 is the 
#' mean (or median-Use Preference option) of these values.
#' \item dh2 Annual maximum of 3-day moving average flows. Compute the maximum of a 3-day moving average flow for 
#' each year. dh2 is the mean (or median-Use Preference option) of these values; 
#' \item dh3 Annual maximum of 7-day moving average flows. Compute the maximum of a 7-day moving average flow for 
#' each year. dh3 is the mean (or median-Use Preference option) of these values.
#' \item dh4 Annual maximum of 30-day moving average flows. Compute the maximum of a 30-day moving average flow for 
#' each year. dh4 is the mean (or median-Use Preference option) of these values.
#' \item dh5 Annual maximum of 90-day moving average flows. Compute the maximum of a 90-day moving average flow for 
#' each year. dh5 is the mean (or median-Use Preference option) of these values.
#' \item dh6 Variability of annual maximum daily flows. Compute the standard deviation for the maximum 1-day 
#' moving averages. dh6 is 100 times the standard deviation divided by the mean.
#' \item dh7 Variability of annual maximum of 3-day moving average flows. Compute the standard deviation for the 
#' maximum 3-day moving averages. dh7 is 100 times the standard deviation divided by the mean.
#' \item dh8 Variability of annual maximum of 7-day moving average flows. Compute the standard deviation for the 
#' maximum 7-day moving averages. dh8 is 100 times the standard deviation divided by the mean.
#' \item dh9 Variability of annual maximum of 30-day moving average flows. Compute the standard deviation for the 
#' maximum 30-day moving averages. dh9 is 100 times the standard deviation divided by the mean.
#' \item dh10 Variability of annual maximum of 90-day moving average flows. Compute the standard deviation for the 
#' maximum 90-day moving averages. dh10 is 100 times the standard deviation divided by the mean.
#' \item dh11 Annual maximum of 1-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 1-day moving average flow for each year. dh11 is the mean of these values divided by the median 
#' for the entire record.
#' \item dh12 Annual maximum of 7-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 7-day moving average flow for each year. dh12 is the mean of these values divided by the median 
#' for the entire record.
#' \item dh13 Annual maximum of 30-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 30-day moving average flow for each year. dh13 is the mean of these values divided by the median 
#' for the entire record.
#' \item dh14 Flood duration. Compute the mean of the mean monthly flow values. Find the 95th percentile for the 
#' mean monthly flows. dh14 is the 95th percentile value divided by the mean of the monthly means.
#' \item dh15 High flow pulse duration. Compute the average duration for flow events with flows above a threshold equal 
#' to the 75th percentile value for each year in the flow record. dh15 is the median of the yearly average durations.
#' \item dh16 Variability in high flow pulse duration. Compute the standard deviation for the yearly average high pulse 
#' durations. dh16 is 100 times the standard deviation divided by the mean of the yearly average high pulse durations.
#' \item dh17 High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' the median flow value for the entire flow record. dh17 is the mean duration 
#' of the events.
#' \item dh18 High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' three times the median flow value for the entire flow record. dh18 is the mean 
#' duration of the events.
#' \item dh19 High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' seven times the median flow value for the entire flow record. dh19 is the mean
#' duration of the events .
#' \item dh20 High flow duration. Compute the 75th percentile value for the entire flow record. Compute the average 
#' duration of flow events with flows above a threshold equal to the 75th percentile value for the median annual 
#' flows. dh20 is the average duration of the events.
#' \item dh21 High flow duration. Compute the 25th percentile value for the entire flow record. Compute the average 
#' duration of flow events with flows above a threshold equal to the 25th percentile value for the entire set 
#' of flows. dh21 is the average duration of the events. 
#' \item dh22 Flood interval. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Determine the median number of days between flood events for each year. dh22 is the mean (or median-Use 
#' Preference option) of the yearly median number of days between flood events.
#' \item dh23 Flood duration. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Determine the number of days each year that the flow remains above the flood threshold. DH23 is the mean (or 
#' median-Use Preference option) of the number of flood days for years in which floods occur.
#' \item dh24 Flood-free days. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Compute the maximum number of days that the flow is below the threshold for each year. DH24 is the mean 
#' (or median-Use Preference option) of the maximum yearly no-flood days.
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom RcppRoll roll_mean
#' @importFrom stats median na.omit quantile sd
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' yearType = "water"
#' hitDurationHigh(x=x,yearType=yearType)
#' 
hitDurationHigh <- function(x,yearType = "water",digits=3,pref="mean",floodThreshold = NULL,...) {
        
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        
        x$month_val <- lubridate::month(x$date)
        
        #Calculate rollings means for entire period of record
        x$roll3Mean <- RcppRoll::roll_mean(x$discharge,n=3L,fill=NA)
        x$roll7Mean <- RcppRoll::roll_mean(x$discharge,n=7L,fill=NA)
        x$roll30Mean <- RcppRoll::roll_mean(x$discharge,n=30L,fill=NA)
        x$roll90Mean <- RcppRoll::roll_mean(x$discharge,n=90L,fill=NA)
        
        #Calculate max and medians by month and year for statistics
        flowSum_year <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         maxFlow = max(discharge),
                                         medFlow = median(discharge))
        flowSum_yearMon <- dplyr::summarize(dplyr::group_by(x,year_val,month_val),
                                            maxFlow = max(discharge),
                                            medFlow = median(discharge),
                                            meanFlow = mean(discharge))
        maxRollingMean <- dplyr::summarize(dplyr::group_by(x,year_val),
                                           maxRoll3Mean = max(roll3Mean,na.rm=TRUE),
                                           maxRoll7Mean = max(roll7Mean,na.rm=TRUE),
                                           maxRoll30Mean = max(roll30Mean,na.rm=TRUE),
                                           maxRoll90Mean = max(roll90Mean,na.rm=TRUE)
        )
        medFlow <- median(x$discharge)
        
        
        #dh1
        if (pref == "mean") {
                dh1 <- mean(flowSum_year$maxFlow)
        } else {
                dh1 <- median(flowSum_year$maxFlow)
        }
        
        
        #dh2-dh5
        if (pref == "mean") {
                dh2.5 <- apply(maxRollingMean[2:5],
                               mean,
                               MARGIN=2,
                               na.rm=TRUE)
        } else {
                dh2.5 <- apply(maxRollingMean[2:5],
                               median,
                               MARGIN=2,
                               na.rm=TRUE)
        }
        ##Unname
        dh2.5 <- unname(dh2.5)
        
        
        #dh6-10
        dh6 <- (sd(flowSum_year$maxFlow) * 100)/mean(flowSum_year$maxFlow)
        dh7.10 <- apply(maxRollingMean[2:5],
                        function(x) (sd(x,na.rm=TRUE)*100)/mean(x,na.rm=TRUE),
                        MARGIN=2)
        dh7.10 <- unname(dh7.10)
        
        
        #dh11-13
        dh11 <- mean(flowSum_year$maxFlow)/medFlow
        dh12.13 <- dh2.5[c(2,3)]/medFlow 
        
        
        #dh14
        
        quant95 <- quantile(flowSum_yearMon$meanFlow,.95,type=6)
        dh14 <- quant95/mean(flowSum_yearMon$meanFlow)
        dh14 <- unname(dh14)
        
        #dh15.16
        thresh <- quantile(x$discharge,probs=0.75,type=6)
        
        #Define events as sustained flows above the high flow threshold
        x$events <- calcEvents(x=x$discharge,threshold=thresh,type="high")$event
        
        yearlyDurations <- dplyr::summarize(dplyr::group_by(x,year_val),
                                            avgDuration = length(na.omit(events))/n_distinct(na.omit(events))
        )
        
        #Replace NaN with 0
        yearlyDurations$avgDuration[is.nan(yearlyDurations$avgDuration)] <- 0
        
        dh15 <- median(yearlyDurations$avgDuration)
        dh16 <- (sd(yearlyDurations$avgDuration)*100)/mean(yearlyDurations$avgDuration)

        
        #dh17-21 #differs than EflowStats because EflowSTats calculates the mean of yearly means 
        #instead of the mean lfow duration for the entire period of record as the documentation states
        percentiles <- quantile(x$discharge,probs=c(0.25,0.75),type=6)
        dh17 <-  eventDuration(x$discharge,threshold=medFlow,aggType="average")
        dh18 <-  eventDuration(x$discharge,threshold=medFlow*3,aggType="average")
        dh19 <-  eventDuration(x$discharge,threshold=medFlow*7,aggType="average")
        dh20 <-  eventDuration(x$discharge,threshold=percentiles["75%"],aggType= "average")
        dh21 <-  eventDuration(x$discharge,threshold=percentiles["25%"],aggType= "average")
        
        #dh22
        if(!is.null(floodThreshold))
        {

                #Define events as sustained flows above the high flow threshold
                
                ###This may not be doing what it should be since the event duration gets artificially cut short at teh beginning and end of the year
                dh22 <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         duration = eventDuration(discharge,
                                                                  threshold=floodThreshold,
                                                                  type="low",
                                                                  aggType = "average",
                                                                  pref="median"))
                dh23 <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         maxDuration = eventDuration(discharge,
                                                                         threshold=floodThreshold,
                                                                         type="high",
                                                                         aggType="max"))
                dh24 <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         maxDuration = eventDuration(discharge,
                                                                         threshold=floodThreshold,
                                                                         type="low",
                                                                     aggType="max"))
                if(pref=="mean")
                {
                        dh22 <- mean(dh22$duration)
                        dh23 <- mean(dh23$maxDuration)
                        dh24 <- mean(dh24$maxDuration)
                        
                } else {
                        dh22 <- median(dh22$duration)
                        dh23 <- median(dh23$maxDuration)
                        dh24 <- median(dh24$maxDuration)
                }
        } else{
                dh22 <- NA
                dh23 <- NA
                dh24 <- NA
        }
        
        
        #Output stats
        dhOut <- data.frame(indice = c(paste0("dh",1:24)),
                            statistic = c(dh1,
                                          dh2.5,
                                          dh6,
                                          dh7.10,
                                          dh11,
                                          dh12.13,
                                          dh14,
                                          dh15,
                                          dh16,
                                          dh17,
                                          dh18,
                                          dh19,
                                          dh20,
                                          dh21,
                                          dh22,
                                          dh23,
                                          dh24),
                            stringsAsFactors = F
        )
        
        dhOut$statistic <- round(dhOut$statistic,digits=digits)
        
        return(dhOut)
}





