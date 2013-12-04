\name{ma4.11}
\alias{ma4.11}
\title{Function to return a list of the MA4-MA11 hydrologic indicator statistics for a given data frame}
\usage{
  ma4.11(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ma4.11 list of the MA4-MA11 statistics for the given data
  frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates MA4; standard deviation
  of the percentiles of the logs of the entire flow record
  divided by the mean of percentiles of the logs. Compute
  the log10 of the daily flows for the entire record.
  Compute the
  5th,10th,15th,20th,30th,35th,40th,45th,50th,55th,60th,65th,70th,
  75th,80th,85th,90th,95th percentiles for the log10s of
  the flow record. Percentiles are computed by
  interpolating between the ordered (ascending) log10s of
  the flow values. Compute the standard deviation and mean
  for the percentile vaules. Divide the standard deviation
  by the mean. MA5; Skewness of the entire flow record.
  Mean (MA1) divided by median (MA2). MA6; Range in daily
  flows is the ratio of the 10% to 90% exceedence values
  for the entire flow record. Compute the 5 to 95 percent
  exceedence values. Exceedence is computed by
  interpolating between the ordered (descending) values.
  Divide the 10% exceedence value by the 90% value. MA7;
  Same as MA6, for 20% and 80% exceedence values. MA8; Same
  as MA6, for 25% and 75% exceedence values. MA9; Spread in
  daily flows is the ratio of the difference between the
  90th and 10th percentile of the logs of the flow data to
  the log of the median of the entire flow record. MA10;
  Same as MA9, but with 20th and 80th percentiles. MA11;
  Same as MA9, but with 25th and 75th percentiles.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ma4.11(x)
}

