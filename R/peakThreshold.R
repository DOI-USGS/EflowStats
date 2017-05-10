#' Function to return a specified flood threshold using data from the NWISWeb peak values service
#' 
#' This function calculates flood thresholds for specified recurrence intervals. 
#' 
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param peakValues A dataframe containing a vector of date values in the first column and vector of numeric annual peak flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param perc value containing the desired percentile to be calculated
#' @return thresh numeric containing the flood threshold for recurence interval specified by \code{perc}
#' @details Compute the log10 of the daily 
#' flows for the peak annual flow days. Calculate the coefficients for a linear regression equation for logs of peak 
#' annual flow versus logs of average daily flow for peak days. Using the log peak flow for the 1.67-year recurrence 
#' interval (60th percentile, \code{perc=0.6}) as input to the regression equation, predict the log10 of the average daily flow. The 
#' threshold is 10 to the log10 (average daily flow) power (cubic feet per second). for the 5-year recurrence interval 
#' (80th percentile, \code{perc=0.8}), used by indices TL3 and TL4, follow the same process, inputing a different 'perc' value.
#' @importFrom stats na.omit quantile
#' @export
#' @examples
#' \dontrun{
#' library(dataRetrieval)
#' x <- sampleData[c("date","discharge")]
#' sites<-"02178400"
#' peakValues <- readNWISpeak(sites)
#' peakValues <- peakValues[c("peak_dt","peak_va")]
#' peakThreshold(x,peakValues,.6,yearType="water")
#' }
peakThreshold <- function(x,peakValues,perc=0.6,yearType = "water") {
        
        #Check x data input
        x <- dataCheck(x,yearType)
        
        #Check peakValues data input
        if (nrow(peakValues)<=1) {
                stop("peakValues must have a minimum of two annual values")
        }
        
        col1_class = class(peakValues[,1])
        col2_class = class(peakValues[,2])
        
        if(col1_class != "Date" && col2_class != "numeric")
        {
                stop("First column of peakValues must contain a vector of class date.\nSecond column of peakValues must contain a vector of class numeric.") 
        } else if (col1_class != "Date")
        {
                stop("First column of peakValues must contain a vector of class date.") 
        } else if (col2_class != "numeric" & col2_class != "integer")
        {
                stop("Second column of peakValues must contain a vector of class numeric.") 
        }
        
        if(anyNA(peakValues))
        {
                stop("dataframe peakValues cannot contain NA values")
        }
        
        
        names(peakValues) <- c("date","peakQ")
        
        
        if(yearType == "water")
        {
                peakValues$year_val <- waterYear(peakValues$date)
        } else {
                peakValues$year_val <- lubridate::year(peakValues$date)
        }
        
        if(any(duplicated(peakValues$year_val))) {
                warning("peakValues data frame contains multiple peak values for one or more years. Only the maximum annual value will be retained.")
                temp <- dplyr::summarize(dplyr::group_by(peakValues,year_val),
                                         date = date[peakQ == max(peakQ)],
                                         peakQ = max(peakQ))
                peakValues <- dplyr::left_join(peakValues["date"],temp, by="date")
                peakValues <- na.omit(peakValues)
        }
        
        peakDaily <- dplyr::inner_join(x,peakValues[c("date","peakQ")],by="date")
        peakDaily$logQ <- log10(peakDaily$discharge)
        peakDaily$logPeakQ <- log10(peakDaily$peakQ)
        
        dailyMean <- mean(peakDaily$logQ)
        instMean <- mean(peakDaily$logPeakQ)
        
        num <- sum((peakDaily$logQ-dailyMean)*(peakDaily$logPeakQ-instMean))
        denom <- sum((peakDaily$logPeakQ-instMean)*(peakDaily$logPeakQ-instMean))
        
        b <- num/denom
        a <- dailyMean - (b*instMean)
        
        lfcrit <- quantile(peakDaily$logPeakQ,
                           perc,
                           type=6)
        
        lq167 <- a + (b*as.numeric(lfcrit))
        thresh <- 10^(lq167)
        return(thresh)
}
