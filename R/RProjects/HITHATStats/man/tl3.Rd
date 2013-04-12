\name{tl3}
\alias{tl3}
\title{Function to return the TL3 hydrologic indicator statistic for a given data frame}
\usage{
  tl3(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  tl3 numeric containing the max low flow events in a
  period over total low flow events for the given data
  frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the seasonal
  predictability of flow below the 20th percentile for the
  entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
tl3(qfiletempf)
}

