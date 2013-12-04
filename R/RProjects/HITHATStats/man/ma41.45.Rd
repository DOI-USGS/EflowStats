\name{ma41.45}
\alias{ma41.45}
\title{Function to return the MA41.45 hydrologic indicator statistics for a given data frame}
\usage{
  ma41.45(qfiletemp, drain_area)
}
\arguments{
  \item{qfiletemp}{data frame containing a "discharge"
  column containing daily flow values}

  \item{drain_area}{numeric containing the drainage area of
  the selected site}
}
\value{
  ma41.45 list of MA41-MA45 for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates MA41;
  annual runoff. Compute the annual mean daily flows. MA41
  is the mean of the annual means divided by the drainage
  area. MA42; Variability across annual flows. MA42 is the
  maximum annual flow minus the minimum annual flow divided
  by the median annual flow. MA43; Same as MA42, but using
  the 25th and 75th percentiles. MA44; Same as MA42, but
  using the 10th and 90th percentiles. MA45; Skewness in
  the annual flows. MA45 is the mean of the annual flow
  means minus the median of the annual flows divided by the
  median of the annual means.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletemp<-read.csv(load_data)
drain_area<-56.5
ma41.45(qfiletemp,drain_area)
}

