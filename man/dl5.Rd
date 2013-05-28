\name{dl5}
\alias{dl5}
\title{Function to return the DL5 hydrologic indicator statistic for a given data frame}
\usage{
  dl5(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dl5 numeric containing the mean of the annual minimum
  90-day average flows for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of the annual
  minimum 90-day average flows for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl5(qfiletempf)
}

