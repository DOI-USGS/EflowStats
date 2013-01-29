\name{l7Q2}
\alias{l7Q2}
\title{Function to return the l7Q2 value for a given data series}
\usage{
  l7Q2(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing value data for
  one of the chosen timeseries}
}
\value{
  l7Q2 l7Q2 value for the given data frame
}
\description{
  This function accepts a data frame containing daily data
  and returns the l7Q2 value
}
\examples{
data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
qfiletempf<-read.csv(data,stringsAsFactors=FALSE)
l7Q2(qfiletempf)
}

