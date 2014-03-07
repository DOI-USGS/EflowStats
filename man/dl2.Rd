\name{dl2}
\alias{dl2}
\title{Function to return the DL2 hydrologic indicator statistic for a given data frame}
\usage{
dl2(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
dl2 numeric containing DL2 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates DL2; Annual minimum of
3-day moving average flow. Compute the minimum of a 3-day
moving average flow for each year. DL2 is the mean (or
median-Use Preference option) of these values (cubic feet
per second-temporal).
}
\examples{
qfiletempf<-sampleData
dl2(qfiletempf)
}

