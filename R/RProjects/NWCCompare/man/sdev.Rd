\name{sdev}
\alias{sdev}
\title{Function to return the standard deviation for a given data series}
\usage{
  sdev(x)
}
\arguments{
  \item{x}{data frame containing value data for the chosen
  timeseries}
}
\value{
  sdev standard deviation for the given data frame
}
\description{
  This function accepts a data frame containing daily data
  and returns the standard deviation
}
\examples{
data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
qfiletempf<-read.csv(data,stringsAsFactors=FALSE)
sdev(qfiletempf)
}

