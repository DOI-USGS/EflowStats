\name{ra7}
\alias{ra7}
\title{Function to return the RA7 hydrologic indicator statistic for a given data frame}
\usage{
  ra7(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  ra7 numeric containing the median log of discharge change
  for days with negative change for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the median change in log
  of discharge for days with negative change for the entire
  record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ra7(qfiletempf)
}

