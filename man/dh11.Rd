\name{dh11}
\alias{dh11}
\title{Function to return the DH11 hydrologic indicator statistic for a given data frame}
\usage{
  dh11(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dh11 numeric containing the mean annual maximum daily
  flow divided by the median flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean annual maximum
  daily flow divided by the median flow of the entire
  record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh11(qfiletempf)
}

