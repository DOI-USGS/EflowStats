\name{fh7}
\alias{fh7}
\title{Function to return the FH7 hydrologic indicator statistic for a given data frame}
\usage{
fh7(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
fh7 numeric value of FH7 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates FH7; Flood frequency.
Compute the average number of flow events with flows above
a threshold equal to seven times the median flow value for
the entire flow record. FH7 is the average (or median-Use
Preference option) number of events (number of
events/year-temporal).
}
\examples{
qfiletempf<-sampleData
fh7(qfiletempf)
}

