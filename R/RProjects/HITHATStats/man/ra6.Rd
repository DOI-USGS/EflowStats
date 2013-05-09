\name{ra6}
\alias{ra6}
\title{Function to return the RA6 hydrologic indicator statistic for a given data frame}
\usage{
  ra6(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  ra6 numeric containing the median positive log of
  discharge change for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the median positive
  change in log of discharge for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ra6(qfiletempf)
}

