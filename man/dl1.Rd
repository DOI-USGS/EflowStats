\name{dl1}
\alias{dl1}
\title{Function to return the DL1 hydrologic indicator statistic for a given data frame}
\usage{
dl1(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
dl1 numeric containing DL1 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates DL1; Annual minimum daily
flow. Compute the minimum 1-day average flow for each year.
DL1 is the mean (or median-Use Preference option) of these
values (cubic feet per second-temporal).
}
\examples{
qfiletempf<-sampleData
dl1(qfiletempf)
}

