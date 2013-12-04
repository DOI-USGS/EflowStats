\name{dl7}
\alias{dl7}
\title{Function to return the DL7 hydrologic indicator statistic for a given data frame}
\usage{
  dl7(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl7 numeric containing the variability of the annual
  minimum 3-day average flows for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability of the
  annual minimum 3-day average flows for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl7(qfiletempf)
}

