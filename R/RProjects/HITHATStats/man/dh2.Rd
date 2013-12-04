\name{dh2}
\alias{dh2}
\title{Function to return the DH2 hydrologic indicator statistic for a given data frame}
\usage{
  dh2(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dh2 numeric containing the mean of the annual maximum
  3-day moving average flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of the annual
  maximum 3-day moving average flow for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh2(qfiletempf)
}

