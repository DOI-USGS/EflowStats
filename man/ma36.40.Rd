\name{ma36.40}
\alias{ma36.40}
\title{Function to return the MA36-MA40 hydrologic indicator statistics for a given data frame}
\usage{
ma36.40(qfiletemp)
}
\arguments{
  \item{qfiletemp}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
ma36.40 list containing MA36-MA40 for the given data frame
}
\description{
This function accepts a data frame that contains columns
named "discharge" and "month_val" and calculates MA36;
variability across monthly flows. Compute the minimum,
maximum, and mean flows for each month in the entire flow
record.  MA36 is the maximum monthly flow minus the minimum
monthly flow divided by the median monthly flow
(dimensionless-spatial). MA37; Variability across monthly
flows.  Compute the first (25th percentile) and the third
(75th percentile) quartiles (every month in the flow
record).  MA37 is the third quartile minus the first
quartile divided by the median of the monthly means
(dimensionless-spatial). MA38; Variability across monthly
flows.  Compute the 10th and 90th percentiles for the
monthly means (every month in the flow record).  MA38 is
the 90th percentile minus the 10th percentile divided by
the median of the monthly means (dimensionless-spatial).
MA39; Variability across monthly flows.  Compute the
standard deviation for the monthly means.  MA39 is the
standard deviation times 100 divided by the mean of the
monthly means (percent-spatial). MA40; Skewness in the
monthly flows.  MA40 is the mean of the monthly flow means
minus the median of the monthly means divided by the median
of the monthly means (dimensionless-spatial).
}
\examples{
qfiletempf<-sampleData
ma36.40(qfiletempf)
}

