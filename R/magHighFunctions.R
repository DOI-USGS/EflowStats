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
        if(pref = "mean")
        {
                meanmaxbymon <- aggregate(maxbymonyr$maxmo, list(maxbymonyr$Month), FUN = mean, na.rm=TRUE)
                mh1 <- c(mh1_Jan_meanMax = round(meanmaxbymon[1,2],digits=2))
                mh2 <- c(mh2_Feb_meanMax = round(meanmaxbymon[2,2],digits=2))
                mh3 <- c(mh3_Mar_meanMax = round(meanmaxbymon[3,2],digits=2))
                mh4 <- c(mh4_Apr_meanMax = round(meanmaxbymon[4,2],digits=2))
                mh5 <- c(mh5_May_meanMax = round(meanmaxbymon[5,2],digits=2))
                mh6 <- c(mh6_Jun_meanMax = round(meanmaxbymon[6,2],digits=2))
                mh7 <- c(mh7_Jul_meanMax = round(meanmaxbymon[7,2],digits=2))
                mh8 <- c(mh8_Aug_meanMax = round(meanmaxbymon[8,2],digits=2))
                mh9 <- c(mh9_Sep_meanMax = round(meanmaxbymon[9,2],digits=2))
                mh10 <- c(m10_Oct_meanMax = round(meanmaxbymon[10,2],digits=2))
                mh11 <- c(mh11_Nov_meanMax = round(meanmaxbymon[11,2],digits=2))
                mh12 <- c(mh12_Dec_meanMax = round(meanmaxbymon[12,2],digits=2))
                mh1.12 <- c(mh1,mh2,mh3,mh4,mh5,mh6,mh7,mh8,mh9,mh10,mh11,mh12)
                return(mh1.12)
        } else {
                medmaxbymon <- aggregate(maxbymonyr$maxmo, list(maxbymonyr$Month), FUN = median, na.rm=TRUE)
                mh1 <- c(mh1_Jan_medMax = round(medmaxbymon[1,2],digits=2))
                mh2 <- c(mh2_Feb_medMax = round(medmaxbymon[2,2],digits=2))
                mh3 <- c(mh3_Mar_medMax = round(medmaxbymon[3,2],digits=2))
                mh4 <- c(mh4_Apr_medMax = round(medmaxbymon[4,2],digits=2))
                mh5 <- c(mh5_May_medMax = round(medmaxbymon[5,2],digits=2))
                mh6 <- c(mh6_Jun_medMax = round(medmaxbymon[6,2],digits=2))
                mh7 <- c(mh7_Jul_medMax = round(medmaxbymon[7,2],digits=2))
                mh8 <- c(mh8_Aug_medMax = round(medmaxbymon[8,2],digits=2))
                mh9 <- c(mh9_Sep_medMax = round(medmaxbymon[9,2],digits=2))
                mh10 <- c(m10_Oct_medMax = round(medmaxbymon[10,2],digits=2))
                mh11 <- c(mh11_Nov_medMax = round(medmaxbymon[11,2],digits=2))
                mh12 <- c(mh12_Dec_medMax = round(medmaxbymon[12,2],digits=2))
                mh1.12 <- c(mh1,mh2,mh3,mh4,mh5,mh6,mh7,mh8,mh9,mh10,mh11,mh12)
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

#' Function to return the MH21 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MH21, high flow volume index. Compute the average volume for flow events above a threshold equal to 
#' the median flow for the entire record. MH21 is the average volume divided by the median flow for the entire 
#' record (days-temporal).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return mh21 numeric value of MH21 for the given data frame
#' @examples
#' x<-sampleData
#' mh21(x)

mh21.24 <- function(x) {
        #Get median for threhsold mh21
        medThreshold <- median(x$discharge,na.rm=TRUE)
        
        #Make a named vector of exceedence thresholds for each statistic
        thresholds <- c(mh21 = medThreshold,
                        mh22 = medThreshold*3,
                        mh23 = medThreshold*7)
        
        #Calculate flows in excess of each threshold
        highFlows <- data.frame(mh21= x$discharge - thresholds["mh21"],
                                mh22 = x$discharge - thresholds["mh22"],
                                mh23 = x$discharge - thresholds["mh23"])
        
        #Define exceedence events as any flow greater than respective threshold,
        #i.e. highFlows is positive
        highFlowEvents <- data.frame(mh21= ifelse(highFlows$mh21 >0,T,F),
                                      mh22 = ifelse(highFlows$mh22 >0,T,F),
                                      mh23 = ifelse(highFlows$mh23 >0,T,F))
        
        
        #Count the number of events where 1 event is all consecutive flow values
        #above the respective threshold
        numEvents <-list(mh21 = rle(highFlowEvents$mh21)$values,
                         mh22 = rle(highFlowEvents$mh22)$values,
                         mh23 = rle(highFlowEvents$mh23)$values)
        
        #Goes with above to do the actual count
        numEvents <-c(mh21 = length(numEvents$mh21[numEvents$mh21==T]),
                         mh22 = length(numEvents$mh22[numEvents$mh22==T]),
                         mh23 = length(numEvents$mh23[numEvents$mh23==T]))
        
        #Calculate total amount of flow that occured during all exceedence events for 
        #each threshold
        totalFlow <- c(mh21 = sum(highFlows$mh21[highFlows$mh21 > 0]),
                       mh22 = sum(highFlows$mh22[highFlows$mh22 > 0]),
                       mh23 = sum(highFlows$mh23[highFlows$mh23 > 0]))
        
        #Calculate the mh21-23 statistics
        mh21_23 <- totalFlow/numEvents/medThreshold
        return(mh21_23)
}
