#' Indices describing timing of average flow events.
#' @description Calculates 3 indices used to describe the timing of average flow conditions. 
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
#' \item  ta1; Constancy. Constancy is computed via the formulation of Colwell (see example in
#' Colwell, 1974). A matrix of values is compiled where the columns are 11 flow
#' categories and the rows are 365 days of the year (no leap years) defined as either calendar or water year. February 29th is removed on leap years. The cell
#' values are the number of times that a flow falls into a category on each day. The
#' categories are listed below. The row totals, column totals, and grand total are computed. Using the equations for
#' Shannon information theory parameters, constancy is computed as 1-(uncertainty with respect to state)/log10(number of state) 
#' \itemize{
#' \item log(flow) < .1 x log(mean flow)
#' \item .1 x log(mean flow) <= log(flow) < .25 x log(mean flow) 
#' \item .25 x log(mean flow) <= log(flow) < .5 x log(mean flow) 
#' \item .5 x log(mean flow) <= log(flow) < .75 x log(mean flow)
#' \item .75 x log(mean flow) <= log(flow) < 1.0 x log(mean flow)
#' \item 1.0 x log(mean flow) <= log(flow) < 1.25 x log(mean flow) 
#' \item 1.25 x log(mean flow) <= log(flow) < 1.5 x log(mean flow)
#' \item 1.5 x log(mean flow) <= log(flow) < 1.75 x log(mean flow)
#' \item 1.75 x log(mean flow) <= log(flow) < 2.0 x log(mean flow)
#' \item 2.0 x log(mean flow) <= log(flow) < 2.25 x log(mean flow) 
#' \item log(flow) >= 2.25 x log(mean flow)
#' } 
#' \item ta2; Predictability. Predictability is computed from the same matrix as constancy (see
#' example in Colwell, 1974). It is computed as: 1- (uncertainty with respect to interaction of time and state - uncertainty with respect to time)/log10(number of state)
#' \item ta3; Seasonal predictability of flooding. Divide years up into 2-month periods (that is,
#' Oct-Nov, Dec-Jan, and so forth). Count the number of flood days (flow events with
#' flows > 1.67-year flood) in each period over the entire flow record. TA3 is the
#' maximum number of flood days in any one period divided by the total number of
#' flood days. 
#' }
#' @return A data.frame of flow statistics
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' yearType = "water"
#' floodThreshold = 1158
#' calc_timingAverage(x=x,yearType = yearType,floodThreshold =floodThreshold)
calc_timingAverage <- function(x,yearType = "water",wyMonth=10L,digits=3,pref="mean",floodThreshold=NULL,...) {
        #Check data inputs
        x <- validate_data(x,yearType=yearType,wyMonth=wyMonth)
        
        if(isFALSE(x)) stop("input data not valid")
        
        check_preference(pref)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        x$dayom_val <- lubridate::mday(x$date)
        meanFlow <- mean(x$discharge)
        
        # remove all February 29th from the time series to calculate 
        # Colwell matrix
        x <- x[!((x$month_val == 2) & (x$dayom_val == 29)),]

        # #Get a new julian day value that does not count leap years
        # after removing the 29th of Feb, we can count the rows sequentially 
        # for each unique water year; https://stackoverflow.com/a/46613159
        x$day <- sequence(rle(x$year_val)$lengths)
        
        #Calculate the colwell matrix... 
        log_meanFlow = log10(meanFlow)
        x$log_discharge = log10(x$discharge)

        #...using findInterval
        break_pts = c(.1, seq(0.25, 2.25, by=.25)) * log_meanFlow
        x$interv = findInterval(x$log_discharge, sort(break_pts))
        colwellMatrix = table(x$day, x$interv)
        
        #ta1.2
        XJ <- rowSums(colwellMatrix)
        YI <- colSums(colwellMatrix)
        Z <- sum(colwellMatrix)
        XJ_sub <- XJ[XJ>0]
        HX <- -sum((XJ_sub/Z)*log10(XJ_sub/Z))
        YI_sub <- YI[YI>0]
        HY <- -sum((YI_sub/Z)*log10(YI_sub/Z))
        colwell_sub <- colwellMatrix[colwellMatrix>0]
        HXY <- -sum((colwell_sub/Z)*log10(colwell_sub/Z))
        HxY <- HXY - HX
        
        ta1 <- 1-(HY/log10(11))
        ta2 <- 100*(1-(HxY/log10(11)))
        
        #ta3
        countsYearMon <- dplyr::summarize(dplyr::group_by(x,year_val,month_val),
                                          counts = length(discharge[discharge>floodThreshold])
                                          )
        
        countsYearMon <- dplyr::arrange(countsYearMon,year_val,month_val)
        rollSum <- RcppRoll::roll_sum(countsYearMon$counts,n=2)
        ta3 <- max(rollSum)/sum(countsYearMon$counts)
        
        #output results
        taOut <- data.frame(indice = c(paste0("ta",1:3)),
                            statistic = c(ta1,
                                          ta2,
                                          ta3),
                            stringsAsFactors = F
        )
        
        taOut$statistic <- round(taOut$statistic,digits=digits)
        
        return(taOut)
}


