\name{dh18}
\alias{dh18}
\title{Function to return the DH18 hydrologic indicator statistic for a given data frame}
\usage{
dh18(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
dh18 numeric containing DH18 for the given data frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates DH18; High flow duration.
Compute the average duration of flow events with flows
above a threshold equal to three times the median flow
value for the entire flow record. DH18 is the average (or
median - Use Preference option) duration of the events
(days-temporal).
}
\examples{
qfiletempf<-sampleData
dh18(qfiletempf)
}

