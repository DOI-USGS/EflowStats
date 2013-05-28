\name{dh13}
\alias{dh13}
\title{Function to return the DH13 hydrologic indicator statistic for a given data frame}
\usage{
  dh13(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh12 numeric containing the annual 30-day moving average
  maximum flow divided by the median flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the annual 30-day moving
  average maximum flow divided by the median flow of the
  entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh13(qfiletempf)
}

