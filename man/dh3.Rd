\name{dh3}
\alias{dh3}
\title{Function to return the DH3 hydrologic indicator statistic for a given data frame}
\usage{
  dh3(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dh3 numeric containing DH3 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates DH3; Annual maximum of
  7-day moving average flows. Compute the maximum of a
  7-day moving average flow for each year. DH3 is the mean
  (or median-Use Preference option) of these values (cubic
  feet per second-temporal).
}
\examples{
qfiletempf<-sampleData
dh3(qfiletempf)
}

