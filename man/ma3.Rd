\name{ma3}
\alias{ma3}
\title{Function to return the MA3 hydrologic indicator statistic for a given data frame}
\usage{
ma3(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
ma3 numeric value of MA3 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates the mean (or median - use
preference option) of the coefficients of variation
(standard deviation/mean) for each year. Compute the
coefficent of variation for each year of daily flows.
Compute the mean/median of the annual coefficients of
variation (percent).
}
\examples{
qfiletempf<-sampleData
ma3(qfiletempf)
}

