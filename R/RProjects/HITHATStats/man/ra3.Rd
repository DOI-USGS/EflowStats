\name{ra3}
\alias{ra3}
\title{Function to return the RA3 hydrologic indicator statistic for a given data frame}
\usage{
  ra3(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  ra3 numeric containing the mean of the fall rate for the
  given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of the fall
  rate for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ra3(qfiletempf)
}

