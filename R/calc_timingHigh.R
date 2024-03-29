#' Indices describing timing of high flow events.
#' @description Calculates 3 indices used to describe the timing of high flow conditions. 
#' See Table X in the EflowStats package vignette for a full description of indices.   
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param wyMonth A numeric. The month of the year in which the water year starts 
#' (1=January, 12=December). The water year begins on the first day of wyMonth.
#' @param digits A numeric. Number of digits to round indice values
#' @param pref A character of either "mean" or "median", indicating whether to use mean or median. See details.
#' @param floodThreshold Numeric flood threshold as the flow equivalent for a flood recurrence of 1.67 years
#' @param ... Optional arguments needed for \code{calc_allHIT} function
#' @details Descriptions of indices.
#' \itemize{
#' \item  th1 Julian date of annual maximum. Determine the Julian date that the maximum flow occurs for each year. 
#' Transform the dates to relative values on a circular scale (radians or degrees). Compute the x and y components 
#' for each year and average them across all years. Compute the mean angle as the arc tangent of y-mean divided by 
#' x-mean. Transform the resultant angle back to Julian date.
#' \item th2 Variability in Julian date of annual maxima. Compute the coefficient of variation for the mean x and y 
#' components and convert to a date.
#' \item th3 Seasonal predictability of nonflooding. Computed as the maximum proportion of a 365-day year that the 
#' flow is less than the 1.67-year flood threshold and also occurs in all years. Accumulate nonflood days that 
#' span all years. TH3 is maximum length of those flood-free periods divided by 365.  
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
#' floodThreshold = 1158
#' calc_timingHigh(x=x,yearType=yearType,floodThreshold=floodThreshold)
calc_timingHigh <- function(x,yearType = "water",wyMonth=10L,digits=3,pref="mean",floodThreshold=NULL,...) {
        #Check data inputs
        x <- validate_data(x,yearType=yearType,wyMonth=wyMonth)
        
        if(isFALSE(x)) stop("input data not valid")
        
        check_preference(pref)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        x$calendar_day <- lubridate::yday(x$date)
        
        # Calculate max flow value and calendat day of max flow for every year.
        flowSum_year <- dplyr::summarize(dplyr::group_by(x,year_val),
                                            maxFlow = max(discharge),
                                         maxFlowJulDay = min(calendar_day[discharge==max(discharge)])
                                         )
        
        #th1 Convert the max flow julian day to psuedo-radians and take cos (x) and sin (y). 
        flowSum_year$np <- cos(flowSum_year$maxFlowJulDay*2*pi/365.25)
        flowSum_year$mdata <- sin(flowSum_year$maxFlowJulDay*2*pi/365.25)

        # Average accross all years.
        xbar <- mean(flowSum_year$np)
        ybar <- mean(flowSum_year$mdata)
        if (xbar>0) { # if x component is greater than 0, just return the angle.
                th1_temp <- atan(ybar/xbar)*180/pi
        } else if (xbar<0) {# if x component is less than 0 (angles pi/2 to 3pi/2), arctan returns -90 -> 0 -> 90 
                th1_temp <- (atan(ybar/xbar)*180/pi)+180 # but we should return 90 -> 180 -> 270 so add 180 to output.
        } else if (xbar==0 && ybar>0) {
                th1_temp <- 90
        } else if (xbar==0 && ybar<0) {
                th1_temp <- 270
        }
        
        # if th1_temp is negative from above (for 270 or 3pi/2 to 360 or 2pi degrees) just add 360.
        th1_temp <- ifelse(th1_temp<0,th1_temp+360,th1_temp) 
        th1 <- th1_temp*365.25/360
        
        #th2
        th2_a <- sqrt((xbar*xbar)+(ybar*ybar))
        th2_b <- sqrt(2*(1-th2_a))
        th2 <- th2_b*180/pi/360*365.25
        
        #th3
        if(!is.null(floodThreshold))
        {
        dailyMaxFlow <- dplyr::summarize(dplyr::group_by(x,day),
                                         maxFlow = max(discharge))
        
        nonFloodEvents <- find_eventDuration(dailyMaxFlow$maxFlow,
                                        threshold=floodThreshold,
                                        aggType = "none",
                                        type="low",
                                        trim = FALSE)
        if(is.data.frame(nonFloodEvents)){
                maxDuration <- max(nonFloodEvents$duration)
        }else{
                #Case when all flows are greater than floodThreshold
                maxDuration <- 0
        }
        th3 <- maxDuration/365
        } else(th3 <- NA)
        #output results
        thOut <- data.frame(indice = c(paste0("th",1:3)),
                            statistic = c(th1,
                                          th2,
                                          th3),
                            stringsAsFactors = F
        )
        
        thOut$statistic <- round(thOut$statistic,digits=digits)
        
        return(thOut)
}

        
