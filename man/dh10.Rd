\name{dh10}
\alias{dh10}
\title{Function to return the DH10 hydrologic indicator statistic for a given data frame}
\usage{
  dh10(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh10 numeric containing DH10 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates DH10; Variability of
  annual maximum of 90-day moving average flows. Compute
  the standard deviation for the maximum 90-day moving
  averages. DH10 is 100 times the standard deviation
  divided by the mean (percent-spatial).
}
\examples{
qfiletempf<-sampleData
dh10(qfiletempf)
}

