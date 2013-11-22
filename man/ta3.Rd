\name{ta3}
\alias{ta3}
\title{Function to return the TA3 hydrologic indicator statistic for a given data frame}
\usage{
  ta3(qfiletempf, thresh)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{thresh}{numeric containing 1.67-year flood
  threshold calculated by getPeakThresh}
}
\value{
  ta3 numeric containing TA3 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and threshold value obtained using the
  peakdata and peakthresh functions and calculates TA3;
  Seasonal predictability of flooding. Divide years up into
  2-month periods (that is, Oct-Nov, Dec-Jan, and so
  forth). Count the number of flood days (flow events with
  flows > 1.67-year flood) in each period over the entire
  flow record. TA3 is the maximum number of flood days in
  any one period divided by the total number of flood days
  (dimensionless-temporal).
}
\examples{
qfiletempf<-sampleData
ta3(qfiletempf, 1158)
}

