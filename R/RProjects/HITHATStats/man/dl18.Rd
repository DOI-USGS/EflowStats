\name{dl18}
\alias{dl18}
\title{Function to return the DL18 hydrologic indicator statistic for a given data frame}
\usage{
  dl18(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dl18 numeric containing the mean of the annual number of
  zero-flow days for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of the annual
  number of zero-flow days for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl18(qfiletempf)
}

