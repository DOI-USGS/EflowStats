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
#' @importFrom zoo na.approx
#' @importFrom RcppRoll roll_min
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' magLow(x=x)
#' 
magLow <- function(x,yearType = "water",digits=3,drainArea = NULL,pref="mean") {
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
                                         minFlow = min(discharge),
                                         medFlow = median(discharge),
                                         meanFlow = mean(discharge))
        
        flowSum_yearMon <- dplyr::summarize(dplyr::group_by(x,year_val,month_val),
                                            minFlow = min(discharge),
                                            medFlow = median(discharge))
        medFlow <- median(x$discharge)
        
        #ml1-12 indices
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
        
        #ml13
        ml13 <- data.frame(indice = "ml13",
                           statistic = (sd(flowSum_yearMon$minFlow)*100)/mean(flowSum_yearMon$minFlow)
        )
        
        #ml14-16
        ml14 <- mean(flowSum_year$minFlow/flowSum_year$medFlow)
        ml15 <- mean(flowSum_year$minFlow/flowSum_year$meanFlow)
        ml16 <- median(flowSum_year$minFlow/flowSum_year$medFlow)
        ml14.16 <- data.frame(indice = c("ml14","ml15","ml16"),
                              statistic = c(ml14,ml15,ml16))
        
        #ml17-18
        bfibyyear <- dplyr::summarize(dplyr::group_by(x,year_val),
                                      bfi = bfi(discharge))
        
        if (pref == "mean") {
                ml17 <- mean(bfibyyear$bfi)
        } else {
                ml17 <- median(bfibyyear$bfi)
        }
        
        sdbfi <- sd(bfibyyear$bfi)
        meanbfi <- mean(bfibyyear$bfi)
        
        ml18 <- (sdbfi/meanbfi)*100
        
        ml17.18 <- data.frame(indice = c("ml17","ml18"),
                              statistic = c(ml17,ml18))
        
        #ml19
        ratiominmean <- (flowSum_year$minFlow/flowSum_year$meanFlow)
        if (pref == "mean") {
                ml19 <- mean(ratiominmean)*100
        } else {
                ml19 <- median(ratiominmean)*100
        }
        
        ml19 <- data.frame(indice = "ml19",
                           statistic = ml19)
        
        #ml20
        #' This function accepts a data frame that contains a column named "discharge" and 
        #' calculates base flow index ML20. Divide the daily flow record into 5-day blocks. Find the minimum flow for 
        #' each block. Assign the minimum flow as a base flow for that block if 90 percent of that minimum flow is less 
        #' than the minimum flows for the blocks on either side. Otherwise, set it to zero. Fill in the zero values 
        #' using linear interpolation. Compute the total flow for the entire record and the total base flow for the 
        #' entire record. ML20 is the ratio of total base flow to total flow (dimensionless-spatial).
        #' 
        #'
        ##MAke blocking variable to divide flow record into 5 day blocks
        numsets <- floor(nrow(x)/5)
        blocks <- rep(1:numsets,5)
        blocks <- blocks[order(blocks)]
        blocks <- blocks[seq(nrow(x))]
        x$block <- blocks
        ###Remove remainder days that do not form a commplete block
        x<-na.omit(x)
        
        ##Calculate mins for each block
        blockMins <- dplyr::summarize(dplyr::group_by(x,block),
                                      minFlow = min(discharge))
        
        ##Calculate 3 day rolling minimum for blocks
        block3min <- RcppRoll::roll_min(blockMins$minFlow,n=3,by=1,align = "center")
        ###Fill in missing 1st and last day of record caused by 3 day rolling
        block3min <- c(NA,block3min,NA)
        
        ##Check if 0.9 times the minimum flow of each block is less than the 3 day rolling min, i.e. that it is 
        ##less than the minimum of the block on either side of it
        #blockMins <- blockMins[c(-1,-nrow(blockMins)),]
        blockMins$baseflow <- ifelse(blockMins$minFlow*0.9 < block3min,blockMins$minFlow,NA)
        
        ###Fill in NAs at start and end of record for na.approx
        if(is.na(blockMins$baseflow[1]))
        {
                blockMins$baseflow[1] <- blockMins$minFlow[1]
        }
        if(is.na(blockMins$baseflow[nrow(blockMins)]))
        {
                blockMins$baseflow[nrow(blockMins)] <- blockMins$minFlow[nrow(blockMins)]
        }
        
        ##Approximate NA values using linear interpolation
        blockMins$baseflow <- zoo::na.approx(blockMins$baseflow)
        
        
        totalFlow <- sum(x$discharge)
        totalBaseflow <- sum((blockMins$baseflow*5))
        
        ml20 <- totalBaseflow/totalFlow
        
        ml20 <- data.frame(indice = "ml20",
                           statistic = ml20)
        
        
        #ml21
        ml21 <- (sd(flowSum_year$minFlow)*100)/mean(flowSum_year$minFlow)
        ml21 <- data.frame(indice = "ml21",
                           statistic = ml21)
        
        #ml22
        if(!is.null(drainArea))
        {
                if (pref == "mean") {
                        ml22 <- (mean(flowSum_year$minFlow))/drainArea
                } 
                else {
                        ml22 <- (median(flowSum_year$minFlow))/drainArea
                }
        } else (ml22 = NA)
        ml22 <- data.frame(indice = "ml22",
                           statistic = ml22)
        
        
        #Jointogether and return
        mlOut <- dplyr::bind_rows(ml1.12,
                                  ml13,
                                  ml14.16,
                                  ml17.18,
                                  ml19,
                                  ml20,
                                  ml21,
                                  ml22)
        
        return(mlOut)
}



