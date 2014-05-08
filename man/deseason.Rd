\name{deseason}
\alias{deseason}
\title{This is a function to deseasonalize streamflow data in order to compute the 7 statistics of daily streamflow
used by Archfield et al., under revision (June 2013). Input to the function is a
time series of streamflow with date in the format Y-m-d. Data should include date, month_val and discharge}
\usage{
deseason(timeseries)
}
\arguments{
  \item{timeseries}{data frame of daily flow data}
}
\value{
deseason data frame of deseasonalized streamflow
}
\description{
This is a function to deseasonalize streamflow data in
order to compute the 7 statistics of daily streamflow used
by Archfield et al., under revision (June 2013). Input to
the function is a time series of streamflow with date in
the format Y-m-d. Data should include date, month_val and
discharge
}
\examples{
timeseries1<-sampleData
timeseries1<-data.frame(timeseries1$date,timeseries1$discharge,timeseries1$month_val,stringsAsFactors=FALSE)
timeseries1$date<-as.Date(timeseries1$timeseries1.date,"\%m/\%d/\%y")
timeseries1<-data.frame(timeseries1$date,timeseries1$timeseries1.discharge,timeseries1$timeseries1.month_val,stringsAsFactors=FALSE)
colnames(timeseries1)<-c("date","discharge","month_val")
deseason(timeseries1)
}

