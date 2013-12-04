\name{dh21}
\alias{dh21}
\title{Function to return the DH21 hydrologic indicator statistic for a given data frame}
\usage{
  dh21(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh20 list containing the mean duration of high pulse flow
  for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean duration of
  flow events above the 25th percentile for the entire
  record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh21(qfiletempf)
}

