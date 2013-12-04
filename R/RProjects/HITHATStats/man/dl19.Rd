\name{dl19}
\alias{dl19}
\title{Function to return the DL19 hydrologic indicator statistic for a given data frame}
\usage{
  dl19(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl19 numeric containing the variability of the annual
  number of zero-flow days for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability of the
  annual number of zero-flow days for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl19(qfiletempf)
}

