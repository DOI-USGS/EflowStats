\name{plot.monthly.mean}
\alias{plot.monthly.mean}
\title{Function to plot the monthly mean time series for a given daily data series}
\usage{
  plot.monthly.mean(meanmonts, station)
}
\arguments{
  \item{meanmonts}{data frame containing mean monthly
  values}

  \item{station}{station number for plot title}
}
\description{
  This function accepts a data frame containing daily data
  and a site number and returns a graph of mean monthly
  data
}
\examples{
qfiletempf<-sampleData
meanmonts<-monthly.mean.ts(qfiletempf)
plot.monthly.mean(meanmonts,'02178400')
}

