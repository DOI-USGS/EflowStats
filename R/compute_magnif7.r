#' Function to return the magnificent seven statistics for a given data series
#' This is a function to compute the 7 statistics of daily streamflow
#' used by Archfield et al., under revision (June 2013). Input to the function is a
#' time series of streamflow with date as a Date Class. Data should be arranged in 
#' two columns with names:  1) date, 2) discharge
#' 
#' @param timeseries1 data frame of daily flow data
#' @return magnif7 data frame of calculated statistics
#' @export
#' @import lubridate
#' @examples
#' timeseries1<-sampleData
#' mgSeven <- magnifSeven(timeseries1)
magnifSeven<-function(timeseries1)  {

  #Rename columns of timeseries dataframe 
  timeseries<-data.frame("date" = timeseries1$date,
                         "discharge" = timeseries1$discharge,
                         "month_val" = months(timeseries1$date),
                         "year_val" = year(timeseries1$date),
                         stringsAsFactors=FALSE)
                         
  #Subset timeseries to remove NAs
  timeseries<-subset(timeseries, !is.na(timeseries$discharge)) 

  #Compute L-moment ratios for time series so consistent in function
  complmom<-lmom.ub(timeseries$discharge)
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
	jday<-as.numeric(strftime(timeseries$date, "%j"))
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
