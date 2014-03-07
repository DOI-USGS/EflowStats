\name{mh21}
\alias{mh21}
\title{Function to return the MH21 hydrologic indicator statistic for a given data frame}
\usage{
mh21(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
mh21 numeric value of MH21 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates MH21, high flow volume
index. Compute the average volume for flow events above a
threshold equal to the median flow for the entire record.
MH21 is the average volume divided by the median flow for
the entire record (days-temporal).
}
\examples{
qfiletempf<-sampleData
mh21(qfiletempf)
}

