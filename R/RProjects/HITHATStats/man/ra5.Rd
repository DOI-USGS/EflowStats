\name{ra5}
\alias{ra5}
\title{Function to return the RA5 hydrologic indicator statistic for a given data frame}
\usage{
  ra5(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  ra5 numeric containing the ratio of gain days to total
  days for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the ratio of gain days
  to total number of days for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ra5(qfiletempf)
}

