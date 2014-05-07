\name{OtherStats}
\alias{OtherStats}
\title{Function to return the OtherStats statistics for a given data series
This is a function to compute the 7 statistics of daily streamflow. Input to the function is a
time series of streamflow with date in the format Y-m-d. Output is a vector of OtherStats
1) mean, 2) median, 3) cv, 4) cv daily and 5) flow percentiles.}
\usage{
OtherStats(timeseries1)
}
\arguments{
  \item{timeseries1}{data frame of daily flow data}
}
\value{
OtherStats data frame of calculated statistics
}
\description{
Function to return the OtherStats statistics for a given
data series This is a function to compute the 7 statistics
of daily streamflow. Input to the function is a time series
of streamflow with date in the format Y-m-d. Output is a
vector of OtherStats 1) mean, 2) median, 3) cv, 4) cv daily
and 5) flow percentiles.
}
\examples{
timeseries1<-sampleData
OtherStats(timeseries1)
}

