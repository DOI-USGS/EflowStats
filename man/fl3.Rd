\name{fl3}
\alias{fl3}
\title{Function to return the FL3 hydrologic indicator statistic for a given data frame}
\usage{
  fl3(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fl3 numeric containing FL3 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates FL3; Frequency of low
  pulse spells. Compute the average number of flow events
  with flows below a threshold equal to 5 percent of the
  mean flow value for the entire flow record. FL3 is the
  average (or median-Use Preference option) number of
  events (number of events/year-temporal).
}
\examples{
qfiletempf<-sampleData
fl3(qfiletempf)
}

