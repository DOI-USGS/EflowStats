\name{ma2}
\alias{ma2}
\title{Function to return the MA2 hydrologic indicator statistic for a given data frame}
\usage{
ma2(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
ma2 numeric value of the median daily flow for the given
data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates the median of the daily
flow values for the entire record (cubic feet per second)
}
\examples{
qfiletempf<-sampleData
ma2(qfiletempf)
}

