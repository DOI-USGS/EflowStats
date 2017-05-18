#' Indices describing magnitude of the low flow condition.
#' @description Calculates 27 indices used to describe the magnitude of the peak flow condition. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param drainArea A numeric specifying the drainage area. Only required for ml22 statistic. Typically squiare miles, see details.
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param ... Optional arguments needed for \code{hitAllStats} function
#' @details Descriptions of indices.
#' \itemize{
#' \item mh1_12  Requires pref argument to be either "mean" or "median" specifying monthly aggregation function. 
#' Default is "mean". Means (or medians - use preference option) of maximum daily flow values for each month. 
#' For example, mh1 is the mean of all January maximum flow values over the entire record. 
#' \item mh13 variability (coefficient of variation) across minimum monthly flow values. Compute the mean 
#' and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation 
#' times 100 divided by the mean maximum monthly flow for all years.
#' \item ml14 Mean of annual minimum annual flows.  ML14 is the mean of the ratios of minimum annual flows to the median 
#' flow for each year.
#' \item ml15 Low flow index. ML15 is the mean (or median-Use Preference option) of the ratios of minimum annual flows to 
#' the mean flow for each year.
#' \item ml16 Median of annual minimum flows.  ML16 is the median of the ratios of minimum annual flows to the median 
#' flow for each year. 
#' \item ml17 Baseflow 1. Compute the mean annual flows. Compute the minimum of a 7-day moving average flow 
#' for each year and divide them by the mean annual flow for that year. ML17 is the mean (or median-Use 
#' Preference option) of those ratios. 
#' \item ml18 Variability in baseflow 1. Compute the standard deviation for the ratios of minimum 7-day 
#' moving average flows to mean annual flows for each year.  ML18 is the standard deviation times 100 divided by 
#' the mean of the ratios. 
#' \item ml19 Baseflow 2. Compute the ratios of the minimum annual flow to mean annual flow for 
#' each year. ML19 is the mean (or median-Use Preference option) of these ratios times 100.
#' \item ml20 Baseflow 3. Divide the daily flow record into 5-day blocks. Find the minimum flow for 
#' each block. Assign the minimum flow as a base flow for that block if 90 percent of that minimum flow is less 
#' than the minimum flows for the blocks on either side. Otherwise, set it to zero. Fill in the zero values 
#' using linear interpolation. Compute the total flow for the entire record and the total base flow for the 
#' entire record. ML20 is the ratio of total base flow to total flow.
#' \item ml21 Variability across annual minimum flows. Compute the mean and standard deviation for the 
#' annual minimum flows. ML21 is the standard deviation times 100 divided by the mean. 
#' \item ml22 Specific mean annual minimum flow.  ML22 is the mean (or median-Use Preference option) of the 
#' annual minimum flows divided by the drainage area.
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom imputeTS na.interpolation
#' @importFrom RcppRoll roll_min
#' @importFrom stats median na.omit sd
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' drainArea <- 50
#' yearType = "water"
#' hitMagLow(x=x,yearType=yearType,drainArea=drainArea)
#' 
hitMagLow <- function(x,yearType = "water",digits=3,drainArea = NULL,pref="mean",...) {
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
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
        
        ml1.12 <- ml1.12$statistic
        
        #ml13
        ml13 <- (sd(flowSum_yearMon$minFlow)*100)/mean(flowSum_yearMon$minFlow)
        
        
        #ml14-16
        ml14 <- mean(flowSum_year$minFlow/flowSum_year$medFlow)
        ml15 <- mean(flowSum_year$minFlow/flowSum_year$meanFlow)
        ml16 <- median(flowSum_year$minFlow/flowSum_year$medFlow)

        
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

        #ml19
        ratiominmean <- (flowSum_year$minFlow/flowSum_year$meanFlow)
        if (pref == "mean") {
                ml19 <- mean(ratiominmean)*100
        } else {
                ml19 <- median(ratiominmean)*100
        }

        
        #ml20
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
        blockMins$baseflow <- imputeTS::na.interpolation(blockMins$baseflow)
        
        
        totalFlow <- sum(x$discharge)
        totalBaseflow <- sum((blockMins$baseflow*5))
        
        ml20 <- totalBaseflow/totalFlow

        
        #ml21
        ml21 <- (sd(flowSum_year$minFlow)*100)/mean(flowSum_year$minFlow)

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

        
        #Jointogether and return
        mlOut <- data.frame(indice = c(paste0("ml",1:22)),
                            statistic = c(ml1.12,
                                          ml13,
                                          ml14,
                                          ml15,
                                          ml16,
                                          ml17,
                                          ml18,
                                          ml19,
                                          ml20,
                                          ml21,
                                          ml22),
                            stringsAsFactors = F
        )
        
        mlOut$statistic <- round(mlOut$statistic,digits=digits)
        

        return(mlOut)
}



