\name{dh14}
\alias{dh14}
\title{Function to return the DH14 hydrologic indicator statistic for a given data frame}
\usage{
  dh14(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh14 numeric containing the 95th percentile of mean
  monthly flows divided by the mean of mean monthly flows
  for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the 95th percentile of
  mean monthly flows divided by the mean of mean monthly
  flows
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh14(qfiletempf)
}

