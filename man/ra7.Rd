\name{ra7}
\alias{ra7}
\title{Function to return the RA7 hydrologic indicator statistic for a given data frame}
\usage{
  ra7(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  ra7 numeric containing RA7 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates RA7; Change of flow.
  Compute the loge of the flows for the entire flow record.
  Compute the change in log of flow for days in which the
  change is negative for the entire flow record. RA7 is the
  median of these log values (cubic feet per
  second/day-temporal).
}
\examples{
qfiletempf<-sampleData
ra7(qfiletempf)
}

