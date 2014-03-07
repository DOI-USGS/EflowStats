\name{magnifSeven}
\alias{magnifSeven}
\title{This is a function to compute the 7 statistics of daily streamflow
used by Archfield et al., under revision (June 2013). Input to the function is a
time series of streamflow with date in the format Y-m-d. Data should be arranged in
two columns with names:  1) date and 2) discharge.
Created May 29, 2013 and functions are modified from previous versions of this code.}
\usage{
magnifSeven(timeseries1)
}
\arguments{
  \item{timeseries1}{data frame of daily flow data}
}
\value{
magnif7 data frame of calculated statistics
}
\description{
This is a function to compute the 7 statistics of daily
streamflow used by Archfield et al., under revision (June
2013). Input to the function is a time series of streamflow
with date in the format Y-m-d. Data should be arranged in
two columns with names: 1) date and 2) discharge. Created
May 29, 2013 and functions are modified from previous
versions of this code.
}
\examples{
timeseries1<-sampleData
timeseries1<-data.frame(timeseries1$date,timeseries1$discharge,timeseries1$month_val,timeseries1$year_val,stringsAsFactors=FALSE)
timeseries1$date<-as.Date(timeseries1$timeseries1.date,"\%m/\%d/\%y")
timeseries1<-data.frame(timeseries1$date,timeseries1$timeseries1.discharge,timeseries1$timeseries1.month_val,timeseries1$timeseries1.year_val,stringsAsFactors=FALSE)
colnames(timeseries1)<-c("date","discharge","month_val","year_val")
magnifSeven(timeseries1)
}

