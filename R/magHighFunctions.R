#' Function to return the MH1-MH12 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "flow", "year_val" and "month_val" and 
#' calculates the mean (or median-Use Preference option) maximum flows for each month across all years. Compute 
#' the maximum daily flow for each month over the entire flow record. For example, MH1 is the mean of the maximums 
#' of all January flow values over the entire record (cubic feet per second-temporal).
#' 
#' @param x data frame containing "flow", "year_val", and "month_val" columns containing daily flow values, year, and month
#' @return mh1.12 data frame containing the mean or median maximum flows for each month
#' @import dplyr
#' @examples
#' x<-sampleData
#' mh1.12(x)
mh1.12 <- function(x,pref="mean",...) {
        
        x <- dplyr::group_by(x,year_val,month_val)
        mh1.12 <- dplyr::summarize(x,maxFlow = max(flow,na.rm=TRUE))
        mh1.12 <- dplyr::group_by(mh1.12,month_val)
        
        if(pref == "mean")
        {
                mh1.12 <- dplyr::summarize(mh1.12,meanMon = mean(maxFlow,na.rm=TRUE))
        } else {
                mh1.12 <- dplyr::summarize(mh1.12,meanMon = median(maxFlow,na.rm=TRUE))
        }
        mh1.12out <- mh1.12$meanMon
        names(mh1.12out) <- paste0("mh",mh1.12$month_val)
        return(mh1.12out)
        
}

#' Function to return the MH13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "flow", "year_val" and "month_val" and 
#' calculates MH13, variability (coefficient of variation) across maximum monthly flow values. Compute the mean 
#' and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation 
#' times 100 divided by the mean maximum monthly flow for all years (percent-spatial).
#' 
#' @param x data frame containing "flow", "year_val", and "month_val" columns containing daily flow values, year, and month
#' @return mh13 numeric value of MH13 for the given data frame
#' @examples
#' x<-sampleData
#' mh13(x)
mh13 <- function(x,...) {
        x <- dplyr::group_by(x,year_val,month_val)
        maxSum <- dplyr::summarize(x,
                                   maxFlow = max(flow,na.rm=TRUE))
        mh13 <- (sd(maxSum$maxFlow)*100)/mean(maxSum$maxFlow)
        return(mh13)
}

#' Function to return the MH14 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "flow", "year_val" and "month_val" and 
#' calculates MH14, median of annual maximum flows. Compute the annual maximum flows from monthly maximum flows. 
#' Compute the ratio of annual maximum flow to median annual flow for each year. MH14 is the median of these ratios 
#' (dimensionless-temporal).
#' 
#' @param x data frame containing a "flow" column containing daily flow values
#' @return mh14 numeric value of MH14 for the given data frame
#' @examples
#' x<-sampleData
#' mh14(x)
mh14 <- function(x,...) {
        maxmonbymoyr <- aggregate(x$flow, list(x$year_val, 
                                               x$month_val), FUN = max, na.rm=TRUE)
        colnames(maxmonbymoyr) <- c("Year", "Month", "momax")
        maxmonbyyrr <- aggregate(maxmonbymoyr$momax, list(maxmonbymoyr$Year), 
                                 FUN = max, na.rm=TRUE)
        colnames(maxmonbyyrr) <- c("Year", "yrmax")
        medflowbyyr <- aggregate(x$flow, list(x$year_val), 
                                 FUN = median, na.rm=TRUE)
        colnames(medflowbyyr) <- c("Year", "yrmed")
        mh14 <- median(maxmonbyyrr$yrmax/medflowbyyr$yrmed)
        return(mh14)
}

#' Function to return the MH15-MH17 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "flow" and 
#' calculates the high flow flow indexes. 
#' MH15; High flow flow index. Compute the 1-percent exceedence value for the entire data record. MH15 is 
#' the 1-percent exceedence value divided by the median flow for the entire record (dimensionless-spatial). 
#' MH16; Compute the 10-percent exceedence value for the entire data record. MH16 is the 10-percent exceedence 
#' value divided by the median flow for the entire record (dimensionless-spatial). 
#' MH17; Compute the 25-percent exceedence value for the entire data record. MH17 is the 25-percent exceedence 
#' value divided by the median flow for the entire record (dimensionless-spatial). 
#' 
#' @param x data frame containing a "flow" column containing daily flow values
#' @return mh15.17 list of numeric value of MH15, MH16 and MH17 for the given data frame
#' @examples
#' x<-sampleData
#' mh15.17(x)
mh15.17 <- function(x,...) {
        hfcrit <- quantile(x$flow,probs=c(.9,0.99,0.75),type=6)
        mh15.17 <- c(mh15=as.numeric(hfcrit["99%"]/median(x$flow,na.rm=TRUE)),
                     mh16=as.numeric(hfcrit["90%"]/median(x$flow,na.rm=TRUE)),
                     mh17=as.numeric(hfcrit["75%"]/median(x$flow,na.rm=TRUE)))
        return(mh15.17)
}

#' Function to return the MH18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "flow" and "year_val" and 
#' calculates MH18, variability across annual maximum flows. Compute the logs (log10) of the maximum annual flows. 
#' Find the standard deviation and mean for these values. MH18 is the standard deviation times 100 divided by the 
#' mean (percent-spatial).
#' 
#' @param x data frame containing a "flow" column containing daily flow values
#' @examples
#' x<-sampleData
#' mh18(x)
mh18 <- function(x,...) {
        maxbyyr <- aggregate(x$flow,list(x$year_val),FUN=max,na.rm=TRUE)
        colnames(maxbyyr) <- c("Year","yrmax")
        log10maxbyyr <- log10(maxbyyr$yrmax)
        mh18 <- (sd(log10maxbyyr)*100)/mean(log10maxbyyr)
        return(mh18)
}

#' Function to return the MH19 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "flow" and "year_val" and 
#' calculates the skewness in annual maximum flows (dimensionless-spatial). Use the equation:
#' MH19   numerator =   N2 ? sum(qm3)-3N ? sum(qm) ? sum(qm2) + 2 ? (sum(qm))3  
#' denominator = N ? (N-1) ? (N-2) ? stddev3  
#' Where:  N = Number of years
#' qm = Log10 (annual maximum flows)
#' stddev = Standard deviation of the annual maximum flows
#' 
#' @param x data frame containing a "flow" column containing daily flow values
#' @return mh19 numeric value of MH19 for the given data frame
#' @examples
#' x<-sampleData
#' mh19(x)
mh19 <- function(x,...) {
        annmax <- aggregate(x$flow,list(x$year_val),FUN=max,na.rm=TRUE)
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
#' This function accepts a data frame that contains columns named "flow" and "year_val" and 
#' calculates MH20, specific mean annual maximum flow. MH20 is the mean (or median-Use Preference option) of 
#' the annual maximum flows divided by the drainage area (cubic feet per second/square mile-temporal).
#' 
#' @param x data frame containing a "flow" column containing daily flow values
#' @param drainarea numeric value of drainage area for a site
#' @param pref string containing a "mean" or "median" preference
#' @return mh20 numeric value of MH20 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' drainarea<-56.5
#' mh20(qfiletempf,drainarea)
mh20 <- function(x,drainArea,pref = "mean",...) {
        maxbyyr <- aggregate(x$flow,list(x$year_val),FUN=max,na.rm=TRUE)
        colnames(maxbyyr) <- c("Year","yrmax")
        if(!is.null(drainArea))
        {
                if (pref == "median") {
                        mh20 <- median(maxbyyr$yrmax)/drainArea
                } 
                else {
                        mh20 <- mean(maxbyyr$yrmax)/drainArea
                }        
        } else(mh20 <- c(mh20 = NA))
        
        
        return(mh20)
}


#' Function to return the MH21-27 hydrologic indicator statistics for a given flow timeseries
#' 
#' This function accepts a vector of daily flow values and 
#' calculates the MH21 through MH27 statistics.
#' 
#' @param x data frame containing a "flow" column containing daily flow values
#' @return A named numeric vector of MH21 through MH27 indicator statistics
#' @examples
#' x <- sampleData$flow
#' mh21.27(x)
mh21.27 <- function(x,...) {
        
        x <- x$flow
        
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
                totalFlow <- sum(eventData$flow-thresholdMed)
                totalFlow/numEvents/thresholdMed
                
        })
        mh21.23 <- unlist(mh21.23)
        names(mh21.23) <- c("mh21","mh22","mh23")
        
        #Calculate the 24-27 statistics
        mh24.27 <- lapply(eventsList,function(x,...){
                eventMax <- dplyr::group_by(x[c("flow","event")],event)
                eventMax <- dplyr::summarize(eventMax,maxQ = max(flow,na.rm=TRUE))
                eventMax <- na.omit(eventMax)
                mean(eventMax$maxQ,na.rm=TRUE)/thresholdMed
        })
        
        mh24.27 <- unlist(mh24.27)
        names(mh24.27) <- c("mh24","mh25","mh26","mh27")
        
        #Combine into one vector and ouput
        mh21.27 <- c(mh21.23,mh24.27)
        return(mh21.27)
}

