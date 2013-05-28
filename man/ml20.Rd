\name{ml20}
\alias{ml20}
\title{Function to return the ML20 hydrologic indicator statistic for a given data frame}
\usage{
  ml20(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ml20 numeric value of the ratio of total base flow to
  total flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates ML20. Divide the daily
  flow record into 5-day blocks. Find the minimum flow for
  each block. Assign the minimum flow as a base flow for
  that block if 90% of that minimum flow is less than the
  minimum flows for the blocks on eitherside. Otherwise,
  set it to zero. Fill in the zero values using linear
  interpolation. Compute the total flow for the entire
  record and the total base flow for the entire record.
  ML20 is the ratio of total base flow to total flow.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data,stringsAsFactors=FALSE)
ml20(x)
}

