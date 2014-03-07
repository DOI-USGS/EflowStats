\name{mh22}
\alias{mh22}
\title{Function to return the MH22 hydrologic indicator statistic for a given data frame}
\usage{
mh22(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
mh22 numeric value of MH22 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates MH22, high flow volume.
Compute the average volume for flow events above a
threshold equal to three times the median flow for the
entire record. MH22 is the average volume divided by the
median flow for the entire record (days-temporal).
}
\examples{
qfiletempf<-sampleData
mh22(qfiletempf)
}

