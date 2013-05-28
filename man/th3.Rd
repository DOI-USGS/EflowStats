\name{th3}
\alias{th3}
\title{Function to return the TH3 hydrologic indicator statistic for a given data frame}
\usage{
  th3(qfiletempf, thresh)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{thresh}{numeric containing 1.67-year flood
  threshold calculated by getPeakThresh}
}
\value{
  tl4 numeric containing the ratio of maximum duration
  across years of flow below the 60th pctl to days in the
  year for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the predictability of
  flow below the 60th percentile for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
th3(qfiletempf)
}

