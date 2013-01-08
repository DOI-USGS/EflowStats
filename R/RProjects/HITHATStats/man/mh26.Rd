\name{mh26}
\alias{mh26}
\title{Function to return the MH26 hydrologic indicator statistic for a given data frame}
\usage{
  mh26(qfiletempf)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  mh26 numeric value of the mean daily flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates MH26. High peak flow.
  Compute the average peak flow value for flow events above
  a threshold equal to seven times the median for for the
  entire record. MH26 is the average peak flow divided by
  the median flow for the entire record.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
mh26(x)
}

