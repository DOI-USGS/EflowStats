\name{dh7}
\alias{dh7}
\title{Function to return the DH7 hydrologic indicator statistic for a given data frame}
\usage{
  dh7(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh7 numeric containing the variability of the annual
  maximum 3-day moving average flow for the given data
  frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability of the
  annual maximum 3-day moving average flow for the entire
  record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh7(qfiletempf)
}

