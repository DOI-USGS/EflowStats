\name{dh4}
\alias{dh4}
\title{Function to return the DH4 hydrologic indicator statistic for a given data frame}
\usage{
  dh4(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dh4 numeric containing the mean of the annual maximum
  30-day moving average flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of the annual
  maximum 30-day moving average flow for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh4(qfiletempf)
}

