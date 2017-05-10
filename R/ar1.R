#' AR1 correlation coefficient
#' 
#' @description Function to compute the AR(1) correlation coefficient for a given timeseries of discharge
#' 
#' This function accepts a data frame containing daily streamflow data and returns the AR(1) 
#' correlation coefficient
#' 
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @importFrom stats ar
#' @return ar1 AR(1) correlation coefficient
#' @export
#' @examples
#' x <- sampleData[c("date","discharge")]
#' ar1(x)
ar1 <- function(x,yearType="water",digits=3) {
        #First, deseasonalize the time series using the long-term monthly means
        #ds.timeseries<-deseason(data)  
        
        x <- dataCheck(x,yearType=yearType)
        
        x$month_val <- lubridate::month(x$date)
        
        flowSum_Mon <- dplyr::summarize(dplyr::group_by(x,month_val),
                                        meanFlow = mean(discharge))
        
        x <- dplyr::left_join(x,flowSum_Mon,by="month_val")
        x$dsQ <- x$discharge-x$meanFlow
        
        #Fit AR(1) model to deseasonalized data but first standardize deseasonlized time series
        ds_std_flows<-scale(x$dsQ, center = TRUE, scale = TRUE)
        armdl<-ar(ds_std_flows, aic = FALSE, order.max = 1, method="yule-walker")
        ar1<-round(armdl$ar,digits=digits)
        return(ar1)
}