\name{ra4}
\alias{ra4}
\title{Function to return the RA4 hydrologic indicator statistic for a given data frame}
\usage{
ra4(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
ra4 numeric containing RA4 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates RA4; Variability in fall
rate. Compute the standard deviation for the negative flow
changes. RA4 is 100 times the standard deviation divided by
the mean (percent-spatial).
}
\examples{
qfiletempf<-sampleData
ra4(qfiletempf)
}

