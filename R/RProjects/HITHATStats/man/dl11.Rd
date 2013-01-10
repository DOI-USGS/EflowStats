\name{dl11}
\alias{dl11}
\title{Function to return the DL11 hydrologic indicator statistic for a given data frame}
\usage{
  dl11(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl11 numeric containing the mean of annual minimum flows
  divided by the median for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean of annual
  minimum flows divided by the median for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl11(qfiletempf)
}

