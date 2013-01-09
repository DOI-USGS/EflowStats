\name{ra2}
\alias{ra2}
\title{Function to return the RA2 hydrologic indicator statistic for a given data frame}
\usage{
  ra2(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  ra2 numeric containing the variability of the rise rate
  for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability of the
  rise rate for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ra2(qfiletempf)
}

