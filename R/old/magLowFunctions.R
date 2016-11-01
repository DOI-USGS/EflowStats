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
#' @return A vector of selected flow statistics
#' @importFrom lubridate year
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' magHigh(x=x,stats="All")
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
                                            medFlow = median(discharge))
        medFlow <- median(x$discharge)
        
        #mh1-12 indices
        if(pref == "mean")
        {
                ml1.12 <- dplyr::summarize(dplyr::group_by(flowSum_yearMon,month_val),
                                           statistic = mean(minFlow))
        } else {
                ml1.12 <- dplyr::summarize(dplyr::group_by(flowSum_yearMon,month_val),
                                           statistic = median(minFlow))
        }
        ml1.12$month_val <- as.character(paste0("ml",ml1.12$month_val))
        ml1.12 <- dplyr::rename(ml1.12,indice=month_val)
        
        
        
ml1.12 <- function(x,pref="mean",...) {
        minbymonyr <- aggregate(x$flow, list(x$year_val, x$month_val), FUN = min, na.rm=TRUE)
        colnames(minbymonyr) <- c("Year","Month","minmo")
        if(pref == "mean")
        {
                meanminbymon <- aggregate(minbymonyr$minmo, list(minbymonyr$Month), FUN = mean, na.rm=TRUE)
                ml1.12 <- c(ml1_Jan_meanmin = meanminbymon[1,2],
                            ml2_Feb_meanmin = meanminbymon[2,2],
                            ml3_Mar_meanmin = meanminbymon[3,2],
                            ml4_Apr_meanmin = meanminbymon[4,2],
                            ml5_May_meanmin = meanminbymon[5,2],
                            ml6_Jun_meanmin = meanminbymon[6,2],
                            ml7_Jul_meanmin = meanminbymon[7,2],
                            ml8_Aug_meanmin = meanminbymon[8,2],
                            ml9_Sep_meanmin = meanminbymon[9,2],
                            m10_Oct_meanmin = meanminbymon[10,2],
                            ml11_Nov_meanmin = meanminbymon[11,2],
                            ml12_Dec_meanmin = meanminbymon[12,2])
                return(ml1.12)
        } else {
                medianminbymon <- aggregate(minbymonyr$minmo, list(minbymonyr$Month), FUN = median, na.rm=TRUE)
                ml1.12 <- c(ml1_Jan_medianmin = medianminbymon[1,2],
                            ml2_Feb_medianmin = medianminbymon[2,2],
                            ml3_Mar_medianmin = medianminbymon[3,2],
                            ml4_Apr_medianmin = medianminbymon[4,2],
                            ml5_May_medianmin = medianminbymon[5,2],
                            ml6_Jun_medianmin = medianminbymon[6,2],
                            ml7_Jul_medianmin = medianminbymon[7,2],
                            ml8_Aug_medianmin = medianminbymon[8,2],
                            ml9_Sep_medianmin = medianminbymon[9,2],
                            m10_Oct_medianmin = medianminbymon[10,2],
                            ml11_Nov_medianmin = medianminbymon[11,2],
                            ml12_Dec_medianmin = medianminbymon[12,2])
                return(ml1.12)
        }
}

#' Function to return the ML13 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge","month_val" and "year_val" and 
#' calculates the Variability (coefficient of variation) across minimum monthly flow values. Compute the mean 
#' and standard deviation for the minimum monthly flows over the entire flow record. ML13 is the standard deviation 
#' times 100 divided by the mean minimum monthly flow for all years (percent-spatial).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml13 numeric value of ML13 for the given data frame
#' @examples
#' x<-sampleData
#' ml13(x)
ml13 <- function(x) {
        minmonbyyr <- aggregate(x$discharge, list(x$year_val, 
                                                           x$month_val), FUN = min, na.rm=TRUE)
        colnames(minmonbyyr) <- c("Year", "Month", "minmo")
        sdminmonflows <- sd(minmonbyyr$minmo)
        meanminmonflows <- mean(minmonbyyr$minmo)
        ml13 <- c(ml13=(sdminmonflows * 100)/meanminmonflows)
        return(ml13)
}

#' Function to return the ML14-ML16 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains columns named "discharge" and "year_val" and calculates 
#' ML14; Mean of annual minimum annual flows.  ML14 is the mean of the ratios of minimum annual flows to the median 
#' flow for each year (dimensionless-temporal). 
#' ML15; Low flow index.  ML15 is the mean (or median-Use Preference option) of the ratios of minimum annual flows to 
#' the mean flow for each year (dimensionless-temporal). 
#' ML16; Median of annual minimum flows.  ML16 is the median of the ratios of minimum annual flows to the median 
#' flow for each year (dimensionless-temporal). 
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml14.16 list of ml14-ml16 for the given data frame
#' @import dplyr
#' @examples
#' x<-sampleData
#' ml14.16(x)
ml14.16 <- function(x) {
        
        minbyyear <- aggregate(x$discharge, 
                               list(x$year_val), min, na.rm=TRUE)
        medflow <- aggregate(x$discharge, list(x$year_val), 
                             median, na.rm=TRUE)
        meanflow <- aggregate(x$discharge, list(x$year_val), mean, na.rm=TRUE)
        computeml14 <- merge(merge(minbyyear, medflow, by.x="Group.1", by.y="Group.1"),meanflow, by.x="Group.1", by.z="Group.1")
        colnames(computeml14) <- c("year", "minbyyr", "medbyyr", "meanbyyr")
        dfml14 <- computeml14$minbyyr/computeml14$medbyyr
        dfml15 <- computeml14$minbyyr/computeml14$meanbyyr
        ml14 <- mean(dfml14)
        ml16 <- median(dfml14)
        ml15 <- mean(dfml15)
        ml14.16 <- c(ml14=ml14,ml15=ml15,ml16=ml16)
        return(ml14.16)
}
