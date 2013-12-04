\name{dl3}
\alias{dl3}
\title{Function to return the DL3 hydrologic indicator statistic for a given data frame}
\usage{
  dl3(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dl3 numeric containing DL3 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates DL3; Annual minimum of
  7-day moving average flow. Compute the minimum of a 7-day
  moving average flow for each year. DL3 is the mean (or
  median-Use Preference option) of these values (cubic feet
  per second-temporal).
}
\examples{
qfiletempf<-sampleData
dl3(qfiletempf)
}

