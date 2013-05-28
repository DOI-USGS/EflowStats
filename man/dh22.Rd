\name{dh22}
\alias{dh22}
\title{Function to return the DH22 hydrologic indicator statistic for a given data frame}
\usage{
  dh22(qfiletempf, thresh)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{thresh}{numeric containing 1.67-year flood
  threshold calculated by getPeakThresh}
}
\value{
  dh22 numeric containing the mean of annual median flood
  intervals for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of annual
  median days between flood events for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data, stringsAsFactors=FALSE)
dh22(qfiletempf, 1158)
}

