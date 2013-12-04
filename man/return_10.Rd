\name{return_10}
\alias{return_10}
\title{Function to return the 10 year return value for a given data series}
\usage{
  return_10(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing value data for
  the chosen timeseries}
}
\value{
  return_10 10-year return value for the given data frame
}
\description{
  This function accepts a data frame containing daily data
  and returns the 10 year return value
}
\examples{
data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
qfiletempf<-read.csv(data,stringsAsFactors=FALSE)
return_10(qfiletempf)
}

