\name{mh27}
\alias{mh27}
\title{Function to return the MH27 hydrologic indicator statistic for a given data frame}
\usage{
  mh27(qfiletempf)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  mh27 numeric value of the mean daily flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates MH27. High peak flow.
  Compute the average peak flow value for flow events above
  a threshold equal to the 75th percentile value for for
  the entire record. MH27 is the average peak flow divided
  by the median flow for the entire record.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
mh27(x)
}

