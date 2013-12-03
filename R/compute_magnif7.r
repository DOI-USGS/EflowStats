#' This is a function to compute the 7 statistics of daily streamflow
#' used by Archfield et al., under revision (June 2013). Input to the function is a
#' time series of streamflow with date in the format Y-m-d. Data should be arranged in 
#' two columns with names:  1) date and 2) discharge.
#' Created May 29, 2013 and functions are modified from previous versions of this code.
#' 
#' @param timeseries1 data frame of daily flow data
#' @return magnif7 data frame of calculated statistics
#' @export
#' @examples
#' load_data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
#' timeseries1<-read.csv(load_data,stringsAsFactors=FALSE)
#' timeseries1<-data.frame(timeseries1$date,timeseries1$discharge,timeseries1$month_val,timeseries1$year_val,stringsAsFactors=FALSE)
#' timeseries1$date<-as.Date(timeseries1$timeseries1.date,"%m/%d/%y")
#' timeseries1<-data.frame(timeseries1$date,timeseries1$timeseries1.discharge,timeseries1$timeseries1.month_val,timeseries1$timeseries1.year_val,stringsAsFactors=FALSE)
#' colnames(timeseries1)<-c("date","discharge","month_val","year_val")
#' magnifSeven(timeseries1)
magnifSeven<-function(timeseries1)  {

  #Rename columns of timeseries dataframe 
  timeseries<-data.frame(timeseries1$date,timeseries1$discharge,timeseries1$month_val,timeseries1$year_val,stringsAsFactors=FALSE)
  colnames(timeseries)<-c("date","flow","month_val","year_val")
  #Subset timeseries to remove NAs
  timeseries<-subset(timeseries,timeseries$flow!='NA') 

  #Compute L-moment ratios for time series so consistent in function
  complmom<-lmom.ub(timeseries$flow)
  lam1<-complmom$L1
  tau2<-complmom$LCV
  tau3<-complmom$TAU3
  tau4<-complmom$TAU4

  #Compute AR(1) correlation coefficienct
  #First, deseasonalize the time series using the long-term monthly means
  ds.timeseries<-deseason(timeseries)  
  #Fit AR(1) model to deseasonalized data but first standardize deseasonlized time series
  ds_std_flows<-scale(ds.timeseries$flow, center = TRUE, scale = TRUE)
  armdl<-ar(ds_std_flows, aic = FALSE, order.max = 1, method="yule-walker")
  ar1<-armdl$ar 
    
  #Compute seasonal factors (amplitude and phase)
  #Compute seasonality variables by first standardizing flows, the fitting relation A*cos(2*pi*t) + B*sin(2*pi*t)
  #1) Get decimal year
  rawdates<-timeseries$date
	dateaschar<-as.character(rawdates)
	jday<-strptime(timeseries$date, "%Y-%m-%d")$yday+1
	decimal_year<-as.numeric(timeseries$year_val)+(jday/365.25)
  #2) Standardize flows
  std_flows<-scale(timeseries$flow, center = TRUE, scale = TRUE)
  #3) Use linear model to fit 
  seasonfit<-lm(std_flows~cos(2*pi*decimal_year)+sin(2*pi*decimal_year))
  seasonA<-as.vector(seasonfit$coefficients[2])
  seasonB<-as.vector(seasonfit$coefficients[3]) 
  #Now compute the amplitude and phase of the seasonal signal
  amplitude<-sqrt((seasonA^2)+(seasonB^2))
  phase<-atan((-seasonB)/seasonA)
  
  #Now output the results
  magnifSeven1<-c(lam1,tau2,tau3,tau4,ar1,amplitude,phase)
  #colnames(magnifSeven1)<-c("lam1","tau2","tau3","tau4","ar1","amplitude","phase")                                      
  magnifSeven<-magnifSeven1
  return(magnifSeven)
}
