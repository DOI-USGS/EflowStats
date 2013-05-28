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
  ma36.40 list containing MA36-MA40 for the given data
  frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "month_val" and calculates MA36;
  variability across monthly flows. Compute the minimum,
  maximum and mean flows for each month in the entire flow
  record. MA36 is the maximum monthly flow minus the
  minimum monthly flow divided by the median monthly flow.
  MA37; Same as MA37, but for the 25th and 75th
  percentiles. MA38; Same as MA37, but for the 10th and
  90th percentiles. MA39; Variability across monthly flows.
  Compute the standard deviation for the monthly means.
  MA39 is the standard deviation times 100 divided by the
  mean of the monthly means. MA40; Skewness in the monthly
  flows. MA40 is the mean of the monthly flow means minus
  the median of the monthly means divided by the median of
  the monthly means.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletemp<-read.csv(load_data)
ma36.40(qfiletemp)
}

