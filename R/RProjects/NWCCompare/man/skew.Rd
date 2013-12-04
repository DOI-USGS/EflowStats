\name{skew}
\alias{skew}
\title{Function to return the skewness for a given data series}
\usage{
  skew(x)
}
\arguments{
  \item{x}{data frame containing value data for the chosen
  timeseries}
}
\value{
  skew skewness for the given data frame
}
\description{
  This function accepts a data frame containing daily data
  and returns the skewness
}
\examples{
data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
qfiletempf<-read.csv(data,stringsAsFactors=FALSE)
skew(qfiletempf)
}

