#' Indices describing timing of low flow events.
#' @description Calculates 3 indices used to describe the timing of high flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param wyMonth The month of the year in which the water year starts.
#' @param digits A numeric. Number of digits to round indice values
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param floodThreshold Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
#' @param ... Optional arguments needed for \code{calc_allHIT} function
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
#' Note: In these definitions, "Julian date" should be interpreted as the count of days starting with 1 on January 
#' first of a given year, ending at 365 or 366 on December 31st.
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @import dplyr
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' yearType = "water"
#' floodThreshold = 1161.38
#' calc_timingLow(x=x,yearType=yearType,floodThreshold=floodThreshold)
calc_timingLow <- function(x,yearType = "water",wyMonth=10L,digits=3,pref="mean",floodThreshold=NULL,...) {
        #Check data inputs
        x <- validate_data(x,yearType=yearType,wyMonth=wyMonth)
        check_preference(pref)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        x$calendar_day <- lubridate::yday(x$date)

        # Calculate min flow value and julian date of min flow for every year.
        flowSum_year <- dplyr::summarize(dplyr::group_by(x,year_val),
                                         minFlow = min(discharge),
                                         minFlowJulDay = min(calendar_day[discharge==min(discharge)])
        )
        
        #tl1 Convert the min flow julian day to psuedo-radians and take cos (x) and sin (y). 
        flowSum_year$np <- cos(flowSum_year$minFlowJulDay*2*pi/365.25)
        flowSum_year$mdata <- sin(flowSum_year$minFlowJulDay*2*pi/365.25)
        
        # Average accross all years.
        xbar <- mean(flowSum_year$np)
        ybar <- mean(flowSum_year$mdata)
        if (xbar>0) { # if x component is greater than 0, just return the angle.
                tl1_temp <- atan(ybar/xbar)*180/pi
        } else if (xbar<0) { # if x component is less than 0 (angles pi/2 to 3pi/2), arctan returns -90 -> 0 -> 90 
                tl1_temp <- (atan(ybar/xbar)*180/pi)+180 # but we should return 90 -> 180 -> 270 so add 180 to output.
        } else if (xbar==0 && ybar>0) { # discontinuity
                tl1_temp <- 90
        } else if (xbar==0 && ybar<0) { # discontinuity
                tl1_temp <- 270
        }
        
        # if tl1_temp is negative from above (for 270 or 3pi/2 to 360 or 2pi degrees) just add 360.
        tl1_temp <- ifelse(tl1_temp<0,tl1_temp+360,tl1_temp)
        tl1 <- round(tl1_temp*365.25/360)
        
        if(tl1 == 0) tl <- 365.25 
        
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
                                             find_events(.$discharge,
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

