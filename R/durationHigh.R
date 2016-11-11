#' Indices describing duration of high flow events.
#' @description Calculates 24 indices used to describe the duration of high flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param stats A character vector containing names of indices to calculate or "All" for all stats. Specific statistics and groups of statistics are listed in details.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param drainArea A numeric specifying the drainage area. Only required for mh20 statistic. See details.
#' @param pref A character indicating whether to use mean or median in monthly aggregation. Only required for ma1_12 statistics. See details.
#' @param peakThresh Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
#' @details Descriptions of indices.
#' \itemize{
#' \item dh1 Annual maximum daily flow. Compute the maximum of a 1-day moving average flow for each year. DH1 is the 
#' mean (or median-Use Preference option) of these values.
#' \item DH2 Annual maximum of 3-day moving average flows. Compute the maximum of a 3-day moving average flow for 
#' each year. DH2 is the mean (or median-Use Preference option) of these values; 
#' \item DH3 Annual maximum of 7-day moving average flows. Compute the maximum of a 7-day moving average flow for 
#' each year. DH3 is the mean (or median-Use Preference option) of these values.
#' \item DH4 Annual maximum of 30-day moving average flows. Compute the maximum of a 30-day moving average flow for 
#' each year. DH4 is the mean (or median-Use Preference option) of these values.
#' \item DH5 Annual maximum of 90-day moving average flows. Compute the maximum of a 90-day moving average flow for 
#' each year. DH5 is the mean (or median-Use Preference option) of these values.
#' \item DH6 Variability of annual maximum daily flows. Compute the standard deviation for the maximum 1-day 
#' moving averages. DH6 is 100 times the standard deviation divided by the mean.
#' \item DH7 Variability of annual maximum of 3-day moving average flows. Compute the standard deviation for the 
#' maximum 3-day moving averages. DH7 is 100 times the standard deviation divided by the mean.
#' \item DH8 Variability of annual maximum of 7-day moving average flows. Compute the standard deviation for the 
#' maximum 7-day moving averages. DH8 is 100 times the standard deviation divided by the mean.
#' \item DH9 Variability of annual maximum of 30-day moving average flows. Compute the standard deviation for the 
#' maximum 30-day moving averages. DH9 is 100 times the standard deviation divided by the mean.
#' \item DH10 Variability of annual maximum of 90-day moving average flows. Compute the standard deviation for the 
#' maximum 90-day moving averages. DH10 is 100 times the standard deviation divided by the mean.
#' \item DH11 Annual maximum of 1-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 1-day moving average flow for each year. DH11 is the mean of these values divided by the median 
#' for the entire record.
#' \item DH12 Annual maximum of 7-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 7-day moving average flow for each year. DH12 is the mean of these values divided by the median 
#' for the entire record.
#' \item DH13 Annual maximum of 30-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 30-day moving average flow for each year. DH13 is the mean of these values divided by the median 
#' for the entire record.
#' \item DH14 Flood duration. Compute the mean of the mean monthly flow values. Find the 95th percentile for the 
#' mean monthly flows. DH14 is the 95th percentile value divided by the mean of the monthly means.
#' \item DH15 High flow pulse duration. Compute the average duration for flow events with flows above a threshold equal 
#' to the 75th percentile value for each year in the flow record. DH15 is the median of the yearly average durations.
#' \item DH16 Variability in high flow pulse duration. Compute the standard deviation for the yearly average high pulse 
#' durations. DH16 is 100 times the standard deviation divided by the mean of the yearly average high pulse durations.
#' \item DH17 High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' the median flow value for the entire flow record. DH17 is the mean duration 
#' of the events.
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
durationHigh <- function(x,yearType = "water",digits=3,drainArea = NULL,pref="mean",floodThreshold = NULL) {
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
                                            medFlow = median(discharge),
                                            meanFlow = mean(discharge))
        maxRollingMean <- dplyr::summarize(dplyr::group_by(x,year_val),
                                           maxRoll3Mean = max(RcppRoll::roll_mean(discharge,n=3L)),
                                           maxRoll7Mean = max(RcppRoll::roll_mean(discharge,n=7L)),
                                           maxRoll30Mean = max(RcppRoll::roll_mean(discharge,n=30L)),
                                           maxRoll90Mean = max(RcppRoll::roll_mean(discharge,n=90L))
        )
        medFlow <- median(x$discharge)

        
        #dh1
        if (pref == "mean") {
                dh1 <- mean(flowSum_year$maxFlow)
        }
        else {
                dh1 <- median(flowSum_year$maxFlow)
        }
        dh1 <- data.frame(indice = "dh1",
                          statistic = dh1)
        
        #dh2-dh5
        if (pref == "mean") {
                dh2.5 <- apply(maxRollingMean[2:5],
                               mean,
                               MARGIN=2)
        } else {
                dh2.5 <- apply(maxRollingMean[2:5],
                               median,
                               MARGIN=2)
        }
        dh2.5 <- data.frame(indice = c("dh2","dh3","dh4","dh5"),
                            statistic = unname(dh2.5))
        
        #dh6-10
        dh6 <- (sd(flowSum_year$maxFlow) * 100)/mean(flowSum_year$maxFlow)
        dh7.10 <- apply(maxRollingMean[2:5],
                        function(x) (sd(x)*100)/mean(x),
                        MARGIN=2)
        dh6.10 <- data.frame(indice=c("dh6","dh7","dh8","dh9","dh10"),
                          statistic = c(dh6,unname(dh7.10))
                          )
        
        #dh11-13
        dh11 <- dh1$statistic/medFlow
        dh12.13 <- dh2.5$statistic[c(2,3)]/medFlow 
        dh11.13 <- data.frame(indice = c("dh11","dh12","dh13"),
                              statistic = c(dh11,dh12.13))
        
        #dh14
        
        quant95 <- quantile(flowSum_yearMon$meanFlow,.95,type=6)
        dh14 <- quant95/mean(flowSum_yearMon$meanFlow)
        dh14 <- unname(dh14)
        dh14 <- data.frame(indice = "dh14",
                           statistic = dh14)
        #dh15.16 #this will differ from EflowStats because the quantile is calculated on the whole record
        #instead of on a yearly basis as it says in the documentation

        yearlyDurations <- dplyr::summarize(dplyr::group_by(x,year_val),
                                     avgDuration = eventDuration(x=discharge,
                                                   threshold = quantile(discharge,probs=0.75,type=6),
                                                   average = TRUE)
        )

        dh15 <- median(yearlyDurations$avgDuration)
        dh16 <- (sd(yearlyDurations$avgDuration)*100)/mean(yearlyDurations$avgDuration)
        dh15.16 <- data.frame(indice=c("dh15","dh16"),
                              statistic = c(dh15,dh16))
        
        #dh17-21 #differs than EflowStats because EflowSTats calculates the mean of yearly means 
        #instead of the mean lfow duration for the entire period of record as the documentation states
        percentiles <- quantile(discharge,probs=c(0.25,0.75),type=6)
        dh17 <-  eventDuration(x$discharge,threshold=medFlow,average=TRUE)
        dh18 <-  eventDuration(x$discharge,threshold=medFlow*3,average=TRUE)
        dh19 <-  eventDuration(x$discharge,threshold=medFlow*7,average=TRUE)
        dh20 <-  eventDuration(x$discharge,threshold=percentiles["75%"],average=TRUE)
        dh21 <-  eventDuration(x$discharge,threshold=percentiles["25%"],average=TRUE)
        
        dh17.21 <- data.frame(indice=c("dh17","dh18","dh18","dh20","dh21"),
                              statistic = c(dh17,dh18,dh19,dh20,dh21))
        
        #dh22
        if(!is.null(floodThreshold))
        {

                dh22 <- dplyr::summarize(dplyr::group_by(x,year_val),
                                 duration = eventDuration(discharge,
                                               threshold=floodThreshold,
                                               type="low",pref="median"))
                if(pref=="mean")
                {
                        dh22 <- mean(dh22$duration)
                } else(dh22 <- median(dh22$duration))
        }else(dh22 <- NA)
        
        dh22 <- data.frame(indice="dh22",
                           statistic = dh22)
        
        #dh23
        if(!is.null(floodThreshold))
        {
                
                dh23 <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         maxDuration = max(eventDuration(discharge,
                                                                  threshold=floodThreshold,
                                                                  type="high",
                                                                  average=FALSE,
                                                                  pref="median")))
                if(pref=="mean")
                {
                        dh23 <- mean(dh23$maxDuration)
                } else(dh23 <- median(dh23$maxDuration))
        }else(dh23 <- NA)
        
        dh23 <- data.frame(indice="dh23",
                           statistic = dh23)
        
        #dh24
        if(!is.null(floodThreshold))
        {
                
                dh24 <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         maxDuration = max(eventDuration(discharge,
                                                                         threshold=floodThreshold,
                                                                         type="low",
                                                                         average=FALSE,
                                                                         pref="median")))
                if(pref=="mean")
                {
                        dh24 <- mean(dh24$maxDuration)
                } else(dh24 <- median(dh24$maxDuration))
        }else(dh23 <- NA)
        
        dh24 <- data.frame(indice="dh24",
                           statistic = dh24)
        
        
        
        
        

                
                
                
                test <- eventDuration(sampleData$discharge,threshold=1158,type="low",average=FALSE)
               #calculate number of days between
               eventDurations <- dplyr::summarize(dplyr::group_by(flowEvents,event),
                                                  duration = length(event)
               )
               
               
                qfiletempf <- qfiletempf[order(qfiletempf$date),]
                lfcrit <- thresh
                noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                                     FUN = median, na.rm=TRUE)
                colnames(noyears) <- c("Year", "momax")
                noyrs <- length(noyears$Year)
                dur <- data.frame(Year = rep(0,nrow(qfiletempf)), dur = rep(1,nrow(qfiletempf)))
                med_yr <- rep(0,noyrs)
                for (i in 1:noyrs) {
                        subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
                        flag <- 0
                        pdur <- 0
                        nevents <- 0
                        for (j in 1:nrow(subsetyr)) {
                                if (subsetyr$discharge[j]<lfcrit) {
                                        flag <- flag+1
                                        nevents <- ifelse(flag==1,nevents+1,nevents)
                                        pdur <- pdur+1
                                } else {
                                        if (flag > 0) {
                                                dur$dur[nevents]<-pdur
                                                dur$Year[nevents]<-subsetyr$wy_val[j]
                                        }
                                        flag <- 0
                                        pdur <- 0
                                }
                        }
                        dur_sub <- dur$dur[dur$Year==subsetyr$wy_val[j]]
                        med_yr[i] <- median(dur_sub)
                }
                
                med_yr[is.na(med_yr)]<-0
                dh22 <- round(mean(med_yr,na.rm=TRUE),digits=2)                
        } else(dh22 <- NA)


        

        
}




