\name{ra5}
\alias{ra5}
\title{Function to return the RA5 hydrologic indicator statistic for a given data frame}
\usage{
ra5(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
ra5 numeric containing RA5 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates RA5; Number of day rises.
Compute the number of days in which the flow is greater
than the previous day. RA5 is the number of positive gain
days divided by the total number of days in the flow record
(dimensionless-spatial).
}
\examples{
qfiletempf<-sampleData
ra5(qfiletempf)
}

