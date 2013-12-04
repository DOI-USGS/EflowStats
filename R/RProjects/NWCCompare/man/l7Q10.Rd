\name{l7Q10}
\alias{l7Q10}
\title{Function to return the l7Q10 value for a given data series}
\usage{
  l7Q10(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing value data for
  one of the chosen timeseries}
}
\value{
  l7Q10 l7Q10 value for the given data frame
}
\description{
  This function accepts a data frame containing daily data
  and returns the l7Q10 value
}
\examples{
data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
qfiletempf<-read.csv(data,stringsAsFactors=FALSE)
l7Q10(qfiletempf)
}

