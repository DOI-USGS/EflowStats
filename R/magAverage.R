#' Indices describing magnitude of the average flow condition.
#' @description Calculates 45 indices used to describe the magnitude of the average flow condition. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param stats A character string containing names of indices to calculate. See details.
#' @param digits A numeric. Number of digits to round indice values
#' @details Descriptions of indices.
#' \itemize{
#' \item ma1  Mean of the daily mean flow values for the entire flow record 
#' \item ma2  Median of the daily mean flow values for the entire flow record 
#' \item  ma3  Mean (or median - use preference option) of the coefficients of 
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
#' \item ma12.23  Means (or medians - use preference option) of monthly flow values. Compute the means for each month over the entire flow record. For example, ma12 is the mean of all January flow values over the entire record. 
#' \item ma24.35  Variability (coefficient of variation) of monthly flow values. Compute the standard deviation for each month in each year over the entire flow record. Divide the standard deviation by the mean for each month. Take the mean (or median - use preference option) of these values for each month across all years. 
#' \item ma36.40  Variability and skewness across monthly flows. ma36 - compute the minimum, maximum and mean flows for each month in the entire flow record. ma36 is the maximum monthly flow minus the minimum monthly flow divided by the median monthly flow. ma37 - compute the first (25th percentile) and the third (75th percentile) quartiles. ma37 is the third quartile minus the first quartile divided by the median of the monthly means. ma38 - compute the 10th and 90th percentiles for the monthly means. ma38 is the 90th percentile minus the 10th percentile divided by the median of the monthly means. ma39 - compute the standard deviation for the monthly means. ma39 is the standard deviation times 100 divided by the mean of the monthly means. ma40 - skewness in the monthly flows. ma40 is the mean of the monthly flow means minus the median of the monthly means divided by the median of the monthly means.
#' \item ma41.45 & Annual runoff and the variability and skewness across annual flows. ma41 - compute the annual mean daily flows. ma41 is the mean of the annual means divided by the drainage area. ma42 is the maximum annual flow minus the minimum annual flow divided by the median annual flow. ma43 - compute the first (25th percentile) and third (75th percentile) quartiles for the annual means. ma43 is the third quartile minus the first quartile divided by the median of the annual means. ma44 - compute the 10th and 90th percentiles for the annual means. ma44 is the 90th percentile minus the 10th percentile divided by the median of the annual means. ma45 - skewness in the annual flows. ma45 is the mean of the annual flow means minus the median of the annual means divided by the median of the annual means.
#' }
#' @return A dataframe
#' @importFrom lubridate(year)
#' @export
#' @examples
#' #Need example
x <- sampleData[c("date","discharge")]
drainArea <- 50
yearType = "water"

magAverage <- function(x,stats = "All",drainArea = NULL, yearType = "water",digits=3) {
        
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
        
        ###rename dataframe for convenient use inside function
        names(x) <- c("date","flow")
        
        ###Get water year value
        if(yearType == "water")
        {
        x$year_val <- waterYear(x$date)
        } else {
                x$year_val <- lubridate::year(x$date)   
        }
        
        x$month_val <- lubridate::month(x$date)

        ###MA1
        ma1 <- c(ma1=mean(x$flow,na.rm=TRUE))
        
        
        ###MA2
        ma2 <- c(ma2=median(x$flow,na.rm=TRUE))
        
        
        ###MA3: There is a mean or median option in the old HITHAT, so returning both here

        ma3_temp <- aggregate(x$flow,
                            list(x$year_val),
                            FUN = cv)
        names(ma3_temp) <- c("year_val","cv")
        
        ma3_median <- c(ma3_median = median(ma3_temp$cv))
        ma3_mean <- c(ma3_mean = mean(ma3_temp$cv))
        rm(ma3_temp)
       
        
        ########################
        ###MA4 through MA11 
        ###Remove sort(x$flow) because I don't see anywhere that flow vector needs to be ordered
        ########################
        
        percentiles <- quantile(x$flow,probs=seq(0.05,0.95,0.05),type=6)
        percMean <- mean(percentiles,na.rm=TRUE)
        percSD <- sd(percentiles,na.rm=TRUE)

        ma4 <- c(ma4 = percMean/percSD*100)

        ma5 <- c(ma5 = ma1/ma2)

        ma6 <- c(ma6 = percentiles["90%"]/percentiles["10%"])

        ma7 <- c(ma7 = percentiles["80%"]/percentiles["20%"])

        ma8 <- c(ma8 = percentiles["75%"]/percentiles["25%"])

        ma9 <- c(ma9 = (percentiles["10%"]-percentiles["90%"])/median(x$flow,na.rm=TRUE))

        ma10 <- c(ma10 = (percentiles["20%"]-percentiles["80%"])/median(x$flow,na.rm=TRUE))

        ma11 <- c(ma11 = (percentiles["25%"]-percentiles["75%"])/median(x$flow,na.rm=TRUE))
        
        
        rm(list=c("percentiles","percMean","percSD"))
        
        ########################
        ###MA12 through MA23
        ########################
        ma12.23_med_temp <- aggregate(x$flow, list(x$month_val),
                            median, na.rm=TRUE)
        names(ma12.23_med_temp) <- c("month","value")
        ma12.23_med_temp$month <- month.abb[ma12.23_med_temp$month]
        ma12.23_med_temp$variable <- paste0("MA",seq(from=12,to=23,by=1),"_",ma12.23_med_temp$month,"_median")
        
        ma12.23_med <- c(ma12.23_med_temp$value)
        names(ma12.23_med) <- ma12.23_med_temp$variable
        
        ma12.23_mean_temp <- aggregate(x$flow, list(x$month_val),
                                 mean, na.rm=TRUE)
        names(ma12.23_mean_temp) <- c("month","value")
        ma12.23_mean_temp$month <- month.abb[ma12.23_mean_temp$month]
        ma12.23_mean_temp$variable <- paste0("MA",seq(from=12,to=23,by=1),"_",ma12.23_mean_temp$month,"_mean")
        
        ma12.23_mean <- c(ma12.23_mean_temp$value)
        names(ma12.23_mean) <- ma12.23_mean_temp$variable
        
        rm(list=c("ma12.23_med_temp","ma12.23_mean_temp"))


        ########################
        ###MA24 through MA35. 
        ########################

        ma24.35_temp <- aggregate(x$flow, list(x$year_val,
                                               x$month_val), FUN = cv)
        names(ma24.35_temp) <- c("year_val","month_val","cv")
        
        ma24.35_median_temp <- aggregate(ma24.35_temp$cv, list(ma24.35_temp$month_val), FUN = median,na.rm=TRUE)
        names(ma24.35_median_temp) <- c("month","value")
        
        ma24.35_median_temp$month <- month.abb[ma24.35_median_temp$month]
        ma24.35_median_temp$variable <- paste0("MA",seq(from=24,to=35,by=1),"_",ma24.35_median_temp$month,"_median")
        
        ma24.35_median <- c(ma24.35_median_temp$value)
        names(ma24.35_median) <- ma24.35_median_temp$variable
                
                
        ma24.35_mean_temp <- aggregate(ma24.35_temp$cv, list(ma24.35_temp$month_val), FUN = mean,na.rm=TRUE)
        names(ma24.35_mean_temp) <- c("month","value")
        
        ma24.35_mean_temp$month <- month.abb[ma24.35_mean_temp$month]
        ma24.35_mean_temp$variable <- paste0("MA",seq(from=24,to=35,by=1),"_",ma24.35_mean_temp$month,"_mean")
        
        ma24.35_mean <- c(ma24.35_mean_temp$value)
        names(ma24.35_mean) <- ma24.35_mean_temp$variable
        
        rm(list=c("ma24.35_temp","ma24.35_median_temp","ma24.35_mean_temp"))
        
        ########################
        ###MA36 through MA40. 
        ########################
        
        monthlyMean <- aggregate(x$flow, list(x$month_val,x$year_val), FUN = mean, na.rm=TRUE)
        colnames(monthlyMean) <- c("month_val","year_val","meanmo")
        
        monthlyMax <- aggregate(x$flow, list(x$month_val,x$year_val), FUN = max, na.rm=TRUE)
        colnames(monthlyMax) <- c("month_val","year_val","maxmo")
        
        monthlyMin <- aggregate(x$flow, list(x$month_val,x$year_val), FUN = min, na.rm=TRUE)
        colnames(monthlyMin) <- c("month_val","year_val","minmo")
        
        percentiles <- quantile(monthlyMean$meanmo,probs=c(0.1,0.25,0.75,0.9),type=6)
        
        medMonthlyFlow <- median(monthlyMean$meanmo)
        meanMonthlyFlow <- mean(monthlyMean$meanmo)
        
        ma36 <- c(ma36 = (max(monthlyMean$meanmo)-min(monthlyMean$meanmo))/medMonthlyFlow)
        ma37 <- c(ma37 = as.numeric((percentiles["75%"]-percentiles["25%"])/medMonthlyFlow))
        ma38 <- c(ma38 = as.numeric((percentiles["90%"]-percentiles["10%"])/medMonthlyFlow))
        ma39 <- c(ma39 = (sd(monthlyMean$meanmo)*100)/meanMonthlyFlow)
        ma40 <- c(ma40 = (meanMonthlyFlow-medMonthlyFlow)/medMonthlyFlow)
        
        rm(list=c("monthlyMean","monthlyMax","monthlyMin","percentiles","medMonthlyFlow","meanMonthlyFlow"))
        
        ########################
        ###MA41 through MA45. 
        ########################
        
        yearlyMean <- aggregate(x$flow, list(x$year_val), FUN = mean, na.rm=TRUE)
        colnames(yearlyMean) <- c("year_val","meanyr")
        
        percentiles <- quantile(yearlyMean$meanyr,probs=c(0.1,0.25,0.75,0.9),type=6)
        
        if(!is.null(drainArea))
        {
                ma41 <- c(ma41 = mean(yearlyMean$meanyr)/drainArea)     
        }
        
        medYearlyFlow <- median(yearlyMean$meanyr, na.rm=TRUE)
        meanYearlyFlow <- mean(yearlyMean$meanyr)
        
        ma42 <- c(ma42 = (max(yearlyMean$meanyr)-min(yearlyMean$meanyr))/medYearlyFlow)
        ma43 <- c(ma43 = as.numeric((percentiles["75%"]-percentiles["25%"])/medYearlyFlow))
        ma44 <- c(ma44 = as.numeric((percentiles["90%"]-percentiles["10%"])/medYearlyFlow))
        ma45 <- c(ma45 = (meanYearlyFlow-medYearlyFlow)/medYearlyFlow)
        
        rm(list=c("yearlyMean","percentiles","medYearlyFlow","meanYearlyFlow"))
        
        return(magAveStats)
}

        


        
        
        