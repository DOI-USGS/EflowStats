\name{dl6}
\alias{dl6}
\title{Function to return the DL6 hydrologic indicator statistic for a given data frame}
\usage{
  dl6(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl6 numeric containing the variability of the annual
  minimum daily average flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability of the
  annual minimum daily average flow for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl6(qfiletempf)
}

