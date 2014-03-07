\name{dl7}
\alias{dl7}
\title{Function to return the DL7 hydrologic indicator statistic for a given data frame}
\usage{
dl7(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
dl7 numeric containing DL7 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates DL7; Variability of annual
minimum of 3-day moving average flow. Compute the standard
deviation for the minimum 3-day moving averages. DL7 is 100
times the standard deviation divided by the mean
(percent-spatial).
}
\examples{
qfiletempf<-sampleData
dl7(qfiletempf)
}

