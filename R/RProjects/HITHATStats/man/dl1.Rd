\name{dl1}
\alias{dl1}
\title{Function to return the DL1 hydrologic indicator statistic for a given data frame}
\usage{
  dl1(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dl1 numeric containing the mean of the annual minimum
  daily flows for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of the annual
  minimum daily flows for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl1(qfiletempf)
}

