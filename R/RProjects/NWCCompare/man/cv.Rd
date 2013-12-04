\name{cv}
\alias{cv}
\title{Function to return the coefficient of variation for a given data series}
\usage{
  cv(x)
}
\arguments{
  \item{x}{data frame containing value data for the chosen
  timeseries}
}
\value{
  cv coefficient of variation for the given data frame
}
\description{
  This function accepts a data frame containing daily data
  and returns the coefficient of variation
}
\examples{
data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
qfiletempf<-read.csv(data,stringsAsFactors=FALSE)
cv(qfiletempf)
}

