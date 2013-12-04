\name{dl16.17}
\alias{dl16.17}
\title{Function to return the DL16 and DL17 hydrologic indicator statistics for a given data frame}
\usage{
  dl16.17(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl16.17 list containing the median duration of low pulse
  flow and variability for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability and
  median average pulse duration for each year of flow below
  the 25th percentile for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl16.17(qfiletempf)
}

