\name{mh24}
\alias{mh24}
\title{Function to return the MH24 hydrologic indicator statistic for a given data frame}
\usage{
  mh24(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  mh24 numeric value of the mean daily flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates MH24. High peak flow.
  Compute the average peak flow value for flow events above
  a threshold equal to the median for for the entire
  record. MH24 is the average peak flow divided by the
  median flow for the entire record.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
mh24(qfiletempf)
}

