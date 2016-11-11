#' Indices describing magnitude of the peak flow condition.
#' @description Calculates 27 indices used to describe the magnitude of the peak flow condition. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param stats A character vector containing names of indices to calculate or "All" for all stats. Specific statistics and groups of statistics are listed in details.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param drainArea A numeric specifying the drainage area. Only required for mh20 statistic. See details.
#' @param pref A character indicating whether to use mean or median in monthly aggregation. Only required for ma1_12 statistics. See details.
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
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' magHigh(x=x)
#' 
magHigh <- function(x,yearType = "water",digits=3,drainArea = NULL,pref="mean") {
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
                warning("dataframe x contains missing values. Missing values will be removed.")
                x <- na.omit(x)
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
                                         maxFlow = max(discharge),
                                         medFlow = median(discharge))
        flowSum_yearMon <- dplyr::summarize(dplyr::group_by(x,year_val,month_val),
                                            maxFlow = max(discharge),
                                            medFlow = median(discharge))
        medFlow <- median(x$discharge)
        
        #mh1-12 indices
        if(pref == "mean")
        {
                mh1.12 <- dplyr::summarize(dplyr::group_by(flowSum_yearMon,month_val),
                                           statistic = mean(maxFlow))
        } else {
                mh1.12 <- dplyr::summarize(dplyr::group_by(flowSum_yearMon,month_val),
                                           statistic = median(maxFlow))
        }
        mh1.12$month_val <- as.character(paste0("mh",mh1.12$month_val))
        mh1.12 <- dplyr::rename(mh1.12,indice=month_val)
        
        #mh13 indice
        mh13 <- data.frame(indice = "mh13",
                           statistic = (sd(flowSum_yearMon$maxFlow)*100)/mean(flowSum_yearMon$maxFlow)
        )
        
        #mh14
        mh14 <- data.frame(indice = "mh14",
                           statistic = median(flowSum_year$maxFlow/flowSum_year$medFlow)
        )
        
        #mh15-17
        hfcrit <- quantile(x$discharge,probs=c(.9,0.99,0.75),type=6)
        mh15.17 <- c(mh15=as.numeric(hfcrit["99%"]/medFlow),
                     mh16=as.numeric(hfcrit["90%"]/medFlow),
                     mh17=as.numeric(hfcrit["75%"]/medFlow))
        mh15.17 <- data.frame(indice = names(mh15.17),
                              statistic = unname(mh15.17))
        
        #mh18
        log10maxbyyr <- log10(flowSum_year$maxFlow)
        mh18 <- data.frame(indice="mh18",
                           statistic = (sd(log10maxbyyr)*100)/mean(log10maxbyyr)
        )
        
        #mh19
        log_disch <- log10(flowSum_year$maxFlow)
        sumq3 <- sum(log_disch^3)
        sumq2 <- sum(log_disch^2)
        sumq <- sum(log_disch)
        num_years <- nrow(flowSum_year)
        qstd <- sd(log_disch)
        mh19 <- c((num_years*num_years*sumq3) - (3*num_years*sumq*sumq2) + (2*sumq*sumq*sumq))/
                (num_years*(num_years-1)*(num_years-2)*qstd*qstd*qstd)
        mh19 <- data.frame(indice = "mh19",
                           statistic = mh19)
        
        
        #mh20
        if(!is.null(drainArea))
        {
                if (pref == "median") {
                        mh20 <- data.frame(indice = "mh20",
                                           statistic = median(flowSum_year$maxFlow)/drainArea
                        )
                } 
                else {
                        mh20 <- data.frame(indice = "mh20",
                                           statistic = mean(flowSum_year$maxFlow)/drainArea
                        )
                }        
        } else(mh20 <- data.frame(indice = "mh20",
                                  statistic = NA)
        )
        
        ###########################
        #mh21-27
        x <- x$discharge
        
        #define thresholds, thresholds are further definer in next code chunk as 
        #a factor of the median (eg 3xmedian)
        thresholdMed <- median(x)
        thresholdQuant <- quantile(x,.75,type=6)
        
        #Identify events above designated thresholds
        eventsList <- list(mh21_24 = calcEvents(x,threshold=thresholdMed),
                           mh22_25 = calcEvents(x,threshold=thresholdMed*3),
                           mh23_26 = calcEvents(x,threshold=thresholdMed*7),
                           mh27 = calcEvents(x,threshold=thresholdQuant))
        
        #Calculate the 21-23 statistics
        mh21.23 <- lapply(eventsList[1:3],function(x,...){
                eventData <- na.omit(x)
                numEvents <- length(unique(eventData$event))
                totalFlow <- sum(eventData$flow-thresholdMed)
                totalFlow/numEvents/thresholdMed
                
        })
        mh21.23 <- unlist(mh21.23)
        names(mh21.23) <- c("mh21","mh22","mh23")
        
        #Calculate the 24-27 statistics
        mh24.27 <- lapply(eventsList,function(x,...){
                eventMax <- dplyr::group_by(x[c("flow","event")],event)
                eventMax <- dplyr::summarize(eventMax,maxQ = max(flow))
                eventMax <- na.omit(eventMax)
                mean(eventMax$maxQ)/thresholdMed
        })
        
        mh24.27 <- unlist(mh24.27)
        names(mh24.27) <- c("mh24","mh25","mh26","mh27")
        
        #Combine into one vector and ouput
        mh21.27 <- c(mh21.23,mh24.27)
        
        mh21.27 <- data.frame(indice = names(mh21.27),
                              statistic = unname(mh21.27))
        ###########################################
        
        
        #Combine all indices into 1 dataframe and return
        mhStats <- dplyr::bind_rows(mh1.12,
                                    mh13,
                                    mh14,
                                    mh15.17,
                                    mh18,
                                    mh19,
                                    mh20,
                                    mh21.27)
        return(mhStats)
}




