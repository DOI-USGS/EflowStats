#' Function to return the magnificent seven statistics for a given data series
#' This is a function to compute the 7 statistics of daily streamflow
#' used by Archfield et al., under revision (June 2013). Input to the function is a
#' time series of streamflow with date in the format Y-m-d. Data should be arranged in 
#' four columns with names:  1) date, 2) discharge, 3) month_val and 4) year_val.
#' 
#' @param timeseries1 data frame of daily flow data
#' @return magnif7 data frame of calculated statistics
#' @export
#' @examples
#' timeseries1<-sampleData
#' timeseries1<-data.frame(timeseries1$date,timeseries1$discharge,timeseries1$month_val,timeseries1$year_val,stringsAsFactors=FALSE)
#' timeseries1$date<-as.Date(timeseries1$timeseries1.date,"%m/%d/%y")
#' timeseries1<-data.frame(timeseries1$date,timeseries1$timeseries1.discharge,timeseries1$timeseries1.month_val,timeseries1$timeseries1.year_val,stringsAsFactors=FALSE)
#' colnames(timeseries1)<-c("date","discharge","month_val","year_val")
#' magnifSeven(timeseries1)
magnifSeven<-function(timeseries1)  {

  #Rename columns of timeseries dataframe 
  timeseries<-data.frame(timeseries1$date,timeseries1$discharge,timeseries1$month_val,timeseries1$year_val,stringsAsFactors=FALSE)
  colnames(timeseries)<-c("date","discharge","month_val","year_val")
  #Subset timeseries to remove NAs
  timeseries<-subset(timeseries,timeseries$discharge!='NA') 

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
