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
  dh24 list containing the mean annual flood days for the
  given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean duration of
  flow events below the 60th percentile for the entire
  record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh24(qfiletempf, 1158)
}

