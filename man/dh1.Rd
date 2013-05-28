\name{dh1}
\alias{dh1}
\title{Function to return the DH1 hydrologic indicator statistic for a given data frame}
\usage{
  dh1(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dh1 numeric containing the mean of the annual maximum
  daily flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of the annual
  maximum daily flow for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh1(qfiletempf)
}

