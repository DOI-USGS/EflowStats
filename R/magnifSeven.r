#' Function to return the magnificent seven statistics for a given data series
#' @param x A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.
#' @param yearType A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @return data.frame of calculated statistics
#' @details This is a function to compute the 7 statistics of daily streamflow
#' used by Archfield et al., under revision (June 2013). 
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom lmom .samlmu
#' @export
#' @examples
#' x<-sampleData[c("date","discharge")]
#' magSeven <- magnifSeven(x)
magnifSeven<-function(x,yearType = "water",digits=3)  {
        
        #Check data inputs
        x <- validate_data(x,yearType)
        
        #calculate some stuff for use later
        x$month_val <- lubridate::month(x$date)
        
        #Compute L-moment ratios for time series so consistent in function
        complmom <- lmom::.samlmu(x$discharge)
        complmom[2] = complmom[2] / complmom[1]
        complmom = round(complmom, digits=2)
        lam1<-complmom[1]
        tau2<-complmom[2]
        tau3<-complmom[3]
        tau4<-complmom[4]
        
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
