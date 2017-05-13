#' Indices describing duration of low flow events.
#' @description Calculates 24 indices used to describe the duration of low flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param ... Optional arguments needed for \code{hitAllStats} function
#' @details Descriptions of indices.
#' \itemize{
#' \item dl1 Annual minimum daily flow. Compute the minimum 1-day average flow for each year. DL1 is the mean 
#' (or median-Use Preference option) of these values.
#' \item dl2 Annual minimum of 3-day moving average flow. Compute the minimum of a 3-day moving average flow for 
#' each year. DL2 is the mean (or median-Use Preference option) of these values. 
#' \item dl3 Annual minimum of 7-day moving average flows. Compute the minimum of a 7-day moving average flow for 
#' each year. DL3 is the mean (or median-Use Preference option) of these values.
#' \item dl4 Annual minimum of 30-day moving average flows. Compute the minimum of a 30-day moving average flow for 
#' each year. DH4 is the mean (or median-Use Preference option) of these values.
#' \item dl5 Annual minimum of 90-day moving average flows. Compute the minimum of a 90-day moving average flow for 
#' each year. DH5 is the mean (or median-Use Preference option) of these values.
#' \item dl6 Variability of annual minimum daily average flow. Compute the standard deviation for the minimum daily 
#' average flow. DL6 is 100 times the standard deviation divided by the mean.
#' \item dl7 Variability of annual minimum of 3-day moving average flows. Compute the standard deviation for the 
#' minimum 3-day moving averages. DL7 is 100 times the standard deviation divided by the mean.
#' \item dl8 Variability of annual minimum of 7-day moving average flows. Compute the standard deviation for the 
#' minimum 7-day moving averages. DL8 is 100 times the standard deviation divided by the mean.
#' \item dl9 Variability of annual minimum of 30-day moving average flows. Compute the standard deviation for the 
#' minimum 30-day moving averages. DL9 is 100 times the standard deviation divided by the mean.
#' \item dl10 Variability of annual minimum of 90-day moving average flows. Compute the standard deviation for the 
#' minimum 90-day moving averages. DH10 is 100 times the standard deviation divided by the mean.
#' \item dl11 Annual minimum daily flow divided by the median for the entire record. Compute the minimum daily flow 
#' for each year. DL11 is the mean of these values divided by the median for the entire record.
#' \item dl12 Annual minimum of 7-day moving average flows divided by the median for the entire record. Compute the 
#' minimum of a 7-day moving average flow for each year. DL12 is the mean of these values divided by the median 
#' for the entire record.
#' \item dl13 Annual minimum of 30-day moving average flows divided by the median for the entire record. Compute the 
#' minimum of a 30-day moving average flow for each year. DL13 is the mean of these values divided by the median 
#' for the entire record.
#' \item dl14 Low exceedence flows. Compute the 75-percent exceedence value for the entire flow record. DL14 is 
#' the exceedence value divided by the median for the entire record.
#' \item dl15 Low exceedence flows. Compute the 90-percent exceedence value for the entire flow record. DL15 is the 
#' exceedence value divided by the median for the entire record.
#' \item dl16 Low flow pulse duration. Compute the average pulse duration for each year for flow events below a 
#' threshold equal to the 25th percentile value for the entire flow record. DL16 is the median of the yearly 
#' average durations.
#' \item dl17 Variability in low pulse duration. Compute the standard deviation for the yearly average low pulse durations. 
#' DL17 is 100 times the standard deviation divided by the mean of the yearly average low pulse durations.
#' \item dl18 Number of zero-flow days. Count the number of zero-flow days for the entire flow record. DL18 is the 
#' mean (or median-Use Preference option) annual number of zero flow days.
#' \item dl19 Variability in the number of zero-flow days. Compute the standard deviation for the annual number of 
#' zero-flow days. DL19 is 100 times the standard deviation divided by the mean annual number of zero-flow days.
#' \item dl20 Number of zero-flow months. While computing the mean monthly flow values, count the number of months 
#' in which there was no flow over the entire flow record.
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom RcppRoll roll_mean
#' @importFrom stats na.omit quantile sd
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' yearType = "water"
#' hitDurationLow(x=x,yearType=yearType)
#' 
hitDurationLow <- function(x,yearType = "water",digits=3,pref="mean",...) {
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        
        #Calculate rolling means
        x$roll3Mean <- RcppRoll::roll_mean(x$discharge,n=3L,fill=NA)
        x$roll7Mean <- RcppRoll::roll_mean(x$discharge,n=7L,fill=NA)
        x$roll30Mean <- RcppRoll::roll_mean(x$discharge,n=30L,fill=NA)
        x$roll90Mean <- RcppRoll::roll_mean(x$discharge,n=90L,fill=NA)
        
        #Calculate max and medians by month and year for statistics
        flowSum_year <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         minFlow = min(discharge),
                                         medFlow = median(discharge))
        flowSum_yearMon <- dplyr::summarize(dplyr::group_by(x,year_val,month_val),
                                            minFlow = min(discharge),
                                            medFlow = median(discharge),
                                            meanFlow = mean(discharge),
                                            totalFlow = sum(discharge))
        minRollingMean <- dplyr::summarize(dplyr::group_by(x,year_val),
                                           minRoll3Mean = min(roll3Mean),
                                           minRoll7Mean = min(roll7Mean),
                                           minRoll30Mean = min(roll30Mean),
                                           minRoll90Mean = min(roll90Mean)
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
                               MARGIN=2,
                               na.rm=TRUE)
        } else {
                dl2.5 <- apply(minRollingMean[2:5],
                               median,
                               MARGIN=2,
                               na.rm=TRUE)
        }
        ##Unname
        dl2.5 <- unname(dl2.5)
        
        #dl6-10
        dl6 <- (sd(flowSum_year$minFlow) * 100)/mean(flowSum_year$minFlow)
        dl7.10 <- apply(minRollingMean[2:5],
                        function(x) (sd(x,na.rm=TRUE)*100)/mean(x,na.rm=TRUE),
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
        
        #dl16.17
        #define low flow threshold
        thresh <- quantile(x$discharge,probs=0.25,type=6)
        
        #Define events as sustained flows below the low flow threshold
        x$events <- calcEvents(x=x$discharge,threshold=thresh,type="low")$event

        yearlyDurations <- dplyr::summarize(dplyr::group_by(x,year_val),
                                            avgDuration = length(na.omit(events))/length(unique(na.omit(events)))
        )
        
        #Replace NaN with 0
        yearlyDurations$avgDuration[is.nan(yearlyDurations$avgDuration)] <- 0
        
        
        dl16 <- median(yearlyDurations$avgDuration)
        dl17 <- (sd(yearlyDurations$avgDuration)*100)/mean(yearlyDurations$avgDuration)
        
        #dl18.19
        yearlyNoFlow <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         noFlowDays = length(discharge[discharge == 0]))
        if (pref == "mean") {
                dl18 <- mean(yearlyNoFlow$noFlowDays)
        } else {
                dl18 <- median(yearlyNoFlow$noFlowDays)
        }
        

        dl19 <- (sd(yearlyNoFlow$noFlowDays)*100)/mean(yearlyNoFlow$noFlowDays)
        
        #dl20
        dl20 <- length(flowSum_yearMon$totalFlow[flowSum_yearMon$totalFlow==0])
        
        #Return stats
        dlOut <- data.frame(indice = c(paste0("dl",1:20)),
                            statistic = c(dl1,
                                          dl2.5,
                                          dl6,
                                          dl7.10,
                                          dl11,
                                          dl12.13,
                                          dl14.15,
                                          dl16,
                                          dl17,
                                          dl18,
                                          dl19,
                                          dl20),
                            stringsAsFactors = F
        )
        
        dlOut$statistic <- round(dlOut$statistic,digits=digits)
        
        return(dlOut)
        
}









