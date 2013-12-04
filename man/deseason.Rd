\name{deseason}
\alias{deseason}
\title{This is a function to deseasonalize streamflow data in order to compute the 7 statistics of daily streamflow
used by Archfield et al., under revision (June 2013). Input to the function is a
time series of streamflow with date in the format Y-m-d. Data should be arranged in
two columns:  1) date and 2) streamflow.
Created May 29, 2013 and functions are modified from previous versions of this code.}
\usage{
  deseason(timeseries)
}
\arguments{
  \item{timeseries1}{data frame of daily flow data}
}
\value{
  deseason data frame of deseasonalized streamflow
}
\description{
  This is a function to deseasonalize streamflow data in
  order to compute the 7 statistics of daily streamflow
  used by Archfield et al., under revision (June 2013).
  Input to the function is a time series of streamflow with
  date in the format Y-m-d. Data should be arranged in two
  columns: 1) date and 2) streamflow. Created May 29, 2013
  and functions are modified from previous versions of this
  code.
}
\examples{
timeseries1<-sampleData
timeseries1<-data.frame(timeseries1$date,timeseries1$discharge,timeseries1$month_val,stringsAsFactors=FALSE)
timeseries1$date<-as.Date(timeseries1$timeseries1.date,"\%m/\%d/\%y")
timeseries1<-data.frame(timeseries1$date,timeseries1$timeseries1.discharge,timeseries1$timeseries1.month_val,stringsAsFactors=FALSE)
colnames(timeseries1)<-c("date","flow","month_val")
deseason(timeseries1)
}

