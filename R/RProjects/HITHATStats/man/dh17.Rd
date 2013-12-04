\name{dh17}
\alias{dh17}
\title{Function to return the DH17 hydrologic indicator statistic for a given data frame}
\usage{
  dh17(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh17 numeric containing the mean duration of high pulse
  flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean average pulse
  duration for each year of flow above the median flow for
  the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh17(qfiletempf)
}

