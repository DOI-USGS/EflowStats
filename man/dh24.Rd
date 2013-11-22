\name{dh24}
\alias{dh24}
\title{Function to return the DH24 hydrologic indicator statistic for a given data frame}
\usage{
  dh24(qfiletempf, thresh)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{thresh}{numeric containing 1.67-year flood
  threshold calculated by getPeakThresh}
}
\value{
  dh24 numeric containing DH24 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and a threshold value obtained using
  the peakdata and peakthresh functions and calculates
  DH24; Flood-free days. Compute the flood threshold as the
  flow equivalent for a flood recurrence of 1.67 years.
  Compute the maximum number of days that the flow is below
  the threshold for each year. DH24 is the mean (or
  median-Use Preference option) of the maximum yearly
  no-flood days (days-temporal).
}
\examples{
qfiletempf<-sampleData
dh24(qfiletempf, 1158)
}

