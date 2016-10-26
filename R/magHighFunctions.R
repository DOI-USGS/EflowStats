#' Function to return the MH1-MH12 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates the mean (or median-Use Preference option) maximum flows for each month across all years. Compute 
#' the maximum daily flow for each month over the entire flow record. For example, MH1 is the mean of the maximums 
#' of all January flow values over the entire record (cubic feet per second-temporal).
#' 
#' @param x data frame containing "discharge", "year_val", and "month_val" columns containing daily flow values, year, and month
#' @return mh1.12 data frame containing the mean or median maximum flows for each month
#' @examples
#' x<-sampleData
#' mh1.12(x)
mh1.12 <- function(x,pref="mean") {
        maxbymonyr <- aggregate(x$discharge, list(x$year_val, x$month_val), FUN = max, na.rm=TRUE)
        colnames(maxbymonyr) <- c("Year","Month","maxmo")
        if(pref == "mean")
        {
                meanmaxbymon <- aggregate(maxbymonyr$maxmo, list(maxbymonyr$Month), FUN = mean, na.rm=TRUE)
                mh1.12 <- c(mh1_Jan_meanMax = meanmaxbymon[1,2],
                            mh2_Feb_meanMax = meanmaxbymon[2,2],
                            mh3_Mar_meanMax = meanmaxbymon[3,2],
                            mh4_Apr_meanMax = meanmaxbymon[4,2],
                            mh5_May_meanMax = meanmaxbymon[5,2],
                            mh6_Jun_meanMax = meanmaxbymon[6,2],
                            mh7_Jul_meanMax = meanmaxbymon[7,2],
                            mh8_Aug_meanMax = meanmaxbymon[8,2],
                            mh9_Sep_meanMax = meanmaxbymon[9,2],
                            m10_Oct_meanMax = meanmaxbymon[10,2],
                            mh11_Nov_meanMax = meanmaxbymon[11,2],
                            mh12_Dec_meanMax = meanmaxbymon[12,2])
                return(mh1.12)
        } else {
                medianmaxbymon <- aggregate(maxbymonyr$maxmo, list(maxbymonyr$Month), FUN = median, na.rm=TRUE)
                mh1.12 <- c(mh1_Jan_medianMax = medianmaxbymon[1,2],
                            mh2_Feb_medianMax = medianmaxbymon[2,2],
                            mh3_Mar_medianMax = medianmaxbymon[3,2],
                            mh4_Apr_medianMax = medianmaxbymon[4,2],
                            mh5_May_medianMax = medianmaxbymon[5,2],
                            mh6_Jun_medianMax = medianmaxbymon[6,2],
                            mh7_Jul_medianMax = medianmaxbymon[7,2],
                            mh8_Aug_medianMax = medianmaxbymon[8,2],
                            mh9_Sep_medianMax = medianmaxbymon[9,2],
                            m10_Oct_medianMax = medianmaxbymon[10,2],
                            mh11_Nov_medianMax = medianmaxbymon[11,2],
                            mh12_Dec_medianMax = medianmaxbymon[12,2])
                return(mh1.12)
        }
}

#' Function to return the MH13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates MH13, variability (coefficient of variation) across maximum monthly flow values. Compute the mean 
#' and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation 
#' times 100 divided by the mean maximum monthly flow for all years (percent-spatial).
#' 
#' @param x data frame containing "discharge", "year_val", and "month_val" columns containing daily flow values, year, and month
#' @return mh13 numeric value of MH13 for the given data frame
#' @examples
#' x<-sampleData
#' mh13(x)
mh13 <- function(x) {
        maxmonbyyr <- aggregate(x$discharge, list(x$year_val, 
                                                  x$month_val), FUN = max, na.rm=TRUE)
        colnames(maxmonbyyr) <- c("Year", "Month", "maxmo")
        sdmaxmonflows <- sd(maxmonbyyr$maxmo)
        meanmaxmonflows <- mean(maxmonbyyr$maxmo)
        mh13 <- (sdmaxmonflows * 100)/meanmaxmonflows
        return(mh13)
}

#' Function to return the MH14 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates MH14, median of annual maximum flows. Compute the annual maximum flows from monthly maximum flows. 
#' Compute the ratio of annual maximum flow to median annual flow for each year. MH14 is the median of these ratios 
#' (dimensionless-temporal).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh14 numeric value of MH14 for the given data frame
#' @examples
#' x<-sampleData
#' mh14(x)
mh14 <- function(x) {
        maxmonbymoyr <- aggregate(x$discharge, list(x$year_val, 
                                                    x$month_val), FUN = max, na.rm=TRUE)
        colnames(maxmonbymoyr) <- c("Year", "Month", "momax")
        maxmonbyyrr <- aggregate(maxmonbymoyr$momax, list(maxmonbymoyr$Year), 
                                 FUN = max, na.rm=TRUE)
        colnames(maxmonbyyrr) <- c("Year", "yrmax")
        medflowbyyr <- aggregate(x$discharge, list(x$year_val), 
                                 FUN = median, na.rm=TRUE)
        colnames(medflowbyyr) <- c("Year", "yrmed")
        mh14 <- maxmonbyyrr$yrmax/medflowbyyr$yrmed
        return(mh14)
}

#' Function to return the MH15-MH17 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flow discharge indexes. 
#' MH15; High flow discharge index. Compute the 1-percent exceedence value for the entire data record. MH15 is 
#' the 1-percent exceedence value divided by the median flow for the entire record (dimensionless-spatial). 
#' MH16; Compute the 10-percent exceedence value for the entire data record. MH16 is the 10-percent exceedence 
#' value divided by the median flow for the entire record (dimensionless-spatial). 
#' MH17; Compute the 25-percent exceedence value for the entire data record. MH17 is the 25-percent exceedence 
#' value divided by the median flow for the entire record (dimensionless-spatial). 
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh15.17 list of numeric value of MH15, MH16 and MH17 for the given data frame
#' @examples
#' x<-sampleData
#' mh15.17(x)
mh15.17 <- function(x) {
        sortq <- sort(x$discharge)
        hfcrit <- quantile(sortq,probs=c(.9,0.99,0.75),type=6)
        mh15.17 <- c(mh15=as.numeric(hfcrit["99%"]/median(x,na.rm=TRUE)),
                     mh16=as.numeric(hfcrit["90%"]/median(x,na.rm=TRUE)),
                     mh17=as.numeric(hfcrit["75%"]/median(x,na.rm=TRUE)))
        return(mh15.17)
}

#' Function to return the MH18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates MH18, variability across annual maximum flows. Compute the logs (log10) of the maximum annual flows. 
#' Find the standard deviation and mean for these values. MH18 is the standard deviation times 100 divided by the 
#' mean (percent-spatial).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @examples
#' x<-sampleData
#' mh18(x)
mh18 <- function(x) {
        maxbyyr <- aggregate(x$discharge,list(x$year_val),FUN=max,na.rm=TRUE)
        colnames(maxbyyr) <- c("Year","yrmax")
        log10maxbyyr <- log10(maxbyyr$yrmax)
        mh18 <- (sd(log10maxbyyr)*100)/mean(log10maxbyyr)
        return(mh18)
}

#' Function to return the MH19 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the skewness in annual maximum flows (dimensionless-spatial). Use the equation:
#' MH19   numerator =   N2 ? sum(qm3)-3N ? sum(qm) ? sum(qm2) + 2 ? (sum(qm))3  
#' denominator = N ? (N-1) ? (N-2) ? stddev3  
#' Where:  N = Number of years
#' qm = Log10 (annual maximum flows)
#' stddev = Standard deviation of the annual maximum flows
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh19 numeric value of MH19 for the given data frame
#' @examples
#' x<-sampleData
#' mh19(x)
mh19 <- function(x) {
        annmax <- aggregate(x$discharge,list(x$year_val),FUN=max,na.rm=TRUE)
        log_disch <- log10(annmax$x)
        sumq3 <- sum(log_disch^3)
        sumq2 <- sum(log_disch^2)
        sumq <- sum(log_disch)
        num_years <- length(unique(x$year_val))
        qstd <- sd(log_disch)
        mh19 <- c((num_years*num_years*sumq3) - (3*num_years*sumq*sumq2) + (2*sumq*sumq*sumq))/
                (num_years*(num_years-1)*(num_years-2)*qstd*qstd*qstd)
        return(mh19)
}

#' Function to return the MH20 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates MH20, specific mean annual maximum flow. MH20 is the mean (or median-Use Preference option) of 
#' the annual maximum flows divided by the drainage area (cubic feet per second/square mile-temporal).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @param drainarea numeric value of drainage area for a site
#' @param pref string containing a "mean" or "median" preference
#' @return mh20 numeric value of MH20 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' drainarea<-56.5
#' mh20(qfiletempf,drainarea)
mh20 <- function(x,drainarea,pref = "mean") {
        maxbyyr <- aggregate(x$discharge,list(x$wy_val),FUN=max,na.rm=TRUE)
        colnames(maxbyyr) <- c("Year","yrmax")
        if (pref == "median") {
                mh20 <- median(maxbyyr$yrmax)/drainarea
        } 
        else {
                mh20 <- mean(maxbyyr$yrmax)/drainarea
        }
        return(mh20)
}


#' Function to return the MH21-27 hydrologic indicator statistics for a given discharge timeseries
#' 
#' This function accepts a vector of daily flow values and 
#' calculates the MH21 through MH27 statistics.
#' 
#' @param x vector of daily flow values
#' @return A named numeric vector of MH21 through MH27 indicator statistics
#' @examples
#' x <- sampleData$discharge
#' mh21.27(x)
mh21.27 <- function(x) {
        #define thresholds, thresholds are further definer in next code chunk as 
        #a factor of the median (eg 3xmedian)
        thresholdMed <- median(x,na.rm=TRUE)
        thresholdQuant <- quantile(x,.75,type=6,na.rm=TRUE)
        
        #Identify events above designated thresholds
        eventsList <- list(mh21_24 = calcHighEvents(x,threshold=thresholdMed),
                           mh22_25 = calcHighEvents(x,threshold=thresholdMed*3),
                           mh23_26 = calcHighEvents(x,threshold=thresholdMed*7),
                           mh27 = calcHighEvents(x,threshold=thresholdQuant))
        
        #Calculate the 21-23 statistics
        mh21.23 <- lapply(eventsList[1:3],function(x,...){
                eventData <- na.omit(x)
                numEvents <- length(unique(eventData$event))
                totalFlow <- sum(eventData$discharge-thresholdMed)
                totalFlow/numEvents/thresholdMed

        })
        mh21.23 <- unlist(mh21.23)
        names(mh21.23) <- c("mh21","mh22","mh23")
        
        #Calculate the 24-27 statistics
        mh24.27 <- lapply(eventsList,function(x,...){
                eventMax <- group_by(x[c("discharge","event")],event)
                eventMax <- summarize(eventMax,maxQ = max(discharge,na.rm=TRUE))
                eventMax <- na.omit(eventMax)
                mean(eventMax$maxQ,na.rm=TRUE)/thresholdMed
        })
        
        mh24.27 <- unlist(mh24.27)
        names(mh24.27) <- c("mh24","mh25","mh26","mh27")
        
        #Combine into one vector and ouput
        mh21.27 <- c(mh21.23,mh24.27)
return(mh21.27)
}

