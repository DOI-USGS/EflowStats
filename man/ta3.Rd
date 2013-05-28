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
  ta3 numeric containing the max floods in a period over
  total floods for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the seasonal
  predictability of floods above the 60th percentile for
  the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ta3(qfiletempf)
}

