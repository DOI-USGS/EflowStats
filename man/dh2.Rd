\name{dh2}
\alias{dh2}
\title{Function to return the DH2 hydrologic indicator statistic for a given data frame}
\usage{
dh2(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
dh2 numeric containing DH2 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates DH2; Annual maximum of
3-day moving average flows. Compute the maximum of a 3-day
moving average flow for each year. DH2 is the mean (or
median-Use Preference option) of these values (cubic feet
per second-temporal).
}
\examples{
qfiletempf<-sampleData
dh2(qfiletempf)
}

