\name{dh6}
\alias{dh6}
\title{Function to return the DH6 hydrologic indicator statistic for a given data frame}
\usage{
  dh6(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh6 numeric containing the variability of the annual
  maximum daily flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability of the
  annual maximum daily flows for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh6(qfiletempf)
}

