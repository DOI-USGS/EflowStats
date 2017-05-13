#' Indices describing timing of low flow events.
#' @description Calculates 3 indices used to describe the timing of high flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param floodThreshold Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
#' @param ... Optional arguments needed for \code{hitAllStats} function
#' @details Descriptions of indices.
#' \itemize{
#' \item  tl1; Julian date of annual minimum. Determine the Julian date that the minimum flow occurs for each water year. 
#' Transform the dates to relative values on a circular scale (radians or degrees). Compute the x and y components 
#' for each year and average them across all years. Compute the mean angle as the arc tangent of y-mean divided by 
#' x-mean. Transform the resultant angle back to Julian date.
#' \item tl2 Variability in Julian date of annual minima. Compute the coefficient of variation for the mean x and y 
#' components and convert to a date.
#' \item tl3 Seasonal predictability of low flow. Divide years up into 2-month periods (that is, Oct-Nov, Dec-Jan, and 
#' so forth). Count the number of low flow events (flow events with flows <= 5 year flood threshold) in each period 
#' over the entire flow record. TL3 is the maximum number of low flow events in any one period divided by the total 
#' number of low flow events.
#' \item tl4 Seasonal predictability of non-low flow. Compute the number of days that flow is above the 5-year flood 
#' threshold as the ratio of number of days to 365 or 366 (leap year) for each year. TL4 is the maximum of the yearly 
#' ratios.
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' yearType = "water"
#' floodThreshold = 1161.38
#' hitTimingLow(x=x,yearType=yearType,floodThreshold=floodThreshold)
hitTimingLow <- function(x,yearType = "water",digits=3,pref="mean",floodThreshold=NULL,...) {
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)

        flowSum_year <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         minFlow = min(discharge),
                                         minFlowJulDay = min(day[discharge==min(discharge)])
        )
        
        #tl1
        flowSum_year$np <- cos(flowSum_year$minFlowJulDay*2*pi/365.25)
        flowSum_year$mdata <- sin(flowSum_year$minFlowJulDay*2*pi/365.25)
        
        xbar <- mean(flowSum_year$np)
        ybar <- mean(flowSum_year$mdata)
        if (xbar>0) {
                tl1_temp <- atan(ybar/xbar)*180/pi
        } else if (xbar<0) {
                tl1_temp <- (atan(ybar/xbar)*180/pi)+180
        } else if (xbar==0 && ybar>0) {
                tl1_temp <- 90
        } else if (xbar==0 && ybar<0) {
                tl1_temp <- 270
        }
        
        tl1_temp <- ifelse(tl1_temp<0,tl1_temp+360,tl1_temp)
        tl1 <- tl1_temp*365.25/360
        
        #tl2
        tl2_a <- sqrt((xbar*xbar)+(ybar*ybar))
        tl2_b <- sqrt(2*(1-tl2_a))
        tl2 <- tl2_b*180/pi/360*365.25
        
        #tl3 - this differs from EFlowStats in that:
        #1) the statistic is calculated based off of a rolling 2 month window isntead of a fixed 2 month block, and 
        #2) events in months from different years are not counted in the total number of events in any 2 momnth period.
        if(!is.null(floodThreshold))
        {
                eventDF <- dplyr::do(dplyr::group_by(x,year_val,month_val),
                                     {
                                             calcEvents(.$discharge,
                                                        threshold=floodThreshold,
                                                        type="low") 
                                     }
                )
                countsYearMon <- dplyr::summarize(dplyr::group_by(eventDF,year_val,month_val),
                                                  eventCount = max(event,na.rm=TRUE)
                )
        
        countsYearMon <- dplyr::arrange(countsYearMon,year_val,month_val)
        rollSum <- RcppRoll::roll_sum(countsYearMon$eventCount,n=2,na.rm=TRUE)
        tl3 <- max(rollSum)/sum(countsYearMon$eventCount)
        
        #tl4
        yearlyRatios <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         ratio = length(discharge[discharge>floodThreshold])/
                                                 max(lubridate::yday(date)))
        
        tl4 <- max(yearlyRatios$ratio)
        } else {
                tl3 <- NA
                tl4 <- NA
        }
        #output results
        tlOut <- data.frame(indice = c(paste0("tl",1:4)),
                            statistic = c(tl1,
                                          tl2,
                                          tl3,
                                          tl4),
                            stringsAsFactors = F
        )
        
        tlOut$statistic <- round(tlOut$statistic,digits=digits)
        
        return(tlOut)
}

