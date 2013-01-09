\name{dl10}
\alias{dl10}
\title{Function to return the DL10 hydrologic indicator statistic for a given data frame}
\usage{
  dl10(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl10 numeric containing the variability of the annual
  minimum 90-day average flows for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability of the
  annual minimum 90-day average flows for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl10(qfiletempf)
}

