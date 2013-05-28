\name{dh23}
\alias{dh23}
\title{Function to return the DH23 hydrologic indicator statistic for a given data frame}
\usage{
  dh23(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh23 list containing the mean annual flood days for the
  given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean duration of
  flow events above the 60th percentile for the entire
  record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh23(qfiletempf)
}

