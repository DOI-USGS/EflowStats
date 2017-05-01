#' Function to return the magnificent seven statistics for a given data series
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @return data.frame of calculated statistics
#' @details This is a function to compute the 7 statistics of daily streamflow
#' used by Archfield et al., under revision (June 2013). 
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom lmomco lmom.ub
#' @export
#' @examples
#' x<-sampleData[c("date","discharge")]
#' magSeven <- magnifSeven(x)
magnifSeven<-function(x,yearType = "water",digits=3)  {
        
        #Check data inputs
        x <- dataCheck(x,yearType)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        
        #Compute L-moment ratios for time series so consistent in function
        complmom<-lmomco::lmom.ub(x$discharge)
        lam1<-round(complmom$L1,digits=2)
        tau2<-round(complmom$LCV,digits=2)
        tau3<-round(complmom$TAU3,digits=2)
        tau4<-round(complmom$TAU4,digits=2)
        
        #Compute AR(1) correlation coefficienct
        ar1v<-ar1(x,yearType = yearType,digits=digits)
        
        seasonality_vars <- seasonality(x)
        amplitude<-seasonality_vars[1]
        phase<-seasonality_vars[2]
        

        #Combine all indices into 1 dataframe and return
        magnif7 <- data.frame(indice = c("lam1","tau2","tau3","tau4","ar1","amplitude","phase"),
                            statistic = c(lam1,tau2,tau3,tau4,ar1v,amplitude,phase),
                            stringsAsFactors = F)

        magnif7$statistic <- round(magnif7$statistic,digits=digits)
        
        return(magnif7)
}
