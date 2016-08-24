#' Function to return the magnificent seven statistics for a given data series
#' This is a function to compute the 7 statistics of daily streamflow
#' used by Archfield et al., under revision (June 2013). Input to the function is a
#' time series of streamflow with date in the format Y-m-d. Data should be arranged in 
#' four columns with names:  1) date, 2) discharge, 3) month_val and 4) year_val.
#' 
#' @param timeseries1 data.frame of daily flow data. See \details for data.frame variables.
#' @return data.frame of calculated statistics
#' @details The \code{timeseries1} data.frame must contain the following named variables:
#' \describe{
#'      \item{date}{A vector of class \code{date} cooresponding to dates of observations}
#'      \item{discharge}{A vector of class \code{numeric} containing discharge measurements}
#' }
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom lmomco lmom.ub
#' @export
#' @examples
#' timeseries<-sampleData[c("date","discharge")]
#' mgSeven <- magnifSeven(timeseries)
magnifSeven<-function(timeseries)  {
        
        timeseries$month_val <- lubridate::month(timeseries$date)
        timeseries$year_val <- lubridate::year(timeseries$date)
        
        #Subset timeseries to remove NAs
        timeseries<-timeseries[!is.na(timeseries$discharge),]
        timeseries <- timeseries[timeseries$discharge != "NA",]
        
        #Compute L-moment ratios for time series so consistent in function
        complmom<-lmomco::lmom.ub(timeseries$discharge)
        lam1<-round(complmom$L1,digits=2)
        tau2<-round(complmom$LCV,digits=2)
        tau3<-round(complmom$TAU3,digits=2)
        tau4<-round(complmom$TAU4,digits=2)
        
        #Compute AR(1) correlation coefficienct
        ar1v<-ar1(timeseries)
        
        #Compute seasonal factors (amplitude and phase)
        #Compute seasonality variables by first standardizing flows, the fitting relation A*cos(2*pi*t) + B*sin(2*pi*t)
        #1) Get decimal year
        rawdates<-timeseries$date
        dateaschar<-as.character(rawdates)
        jday<-strptime(timeseries$date, "%Y-%m-%d")$yday+1
        decimal_year<-as.numeric(timeseries$year_val)+(jday/365.25)
        #2) Standardize flows
        std_flows<-scale(timeseries$discharge, center = TRUE, scale = TRUE)
        #3) Use linear model to fit 
        seasonfit<-lm(std_flows~cos(2*pi*decimal_year)+sin(2*pi*decimal_year))
        seasonA<-as.vector(seasonfit$coefficients[2])
        seasonB<-as.vector(seasonfit$coefficients[3]) 
        #Now compute the amplitude and phase of the seasonal signal
        seasonality_vars <- seasonality(timeseries)
        amplitude<-seasonality_vars[1]
        phase<-seasonality_vars[2]
        
        #Now output the results
        magnifSeven1<-c(lam1,tau2,tau3,tau4,ar1v,amplitude,phase)
        #colnames(magnifSeven1)<-c("lam1","tau2","tau3","tau4","ar1","amplitude","phase")                                      
        magnif7<-magnifSeven1
        return(magnif7)
}
