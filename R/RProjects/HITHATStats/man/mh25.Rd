\name{mh25}
\alias{mh25}
\title{Function to return the MH25 hydrologic indicator statistic for a given data frame}
\usage{
  mh25(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  mh25 numeric value of the mean daily flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates MH25. High peak flow.
  Compute the average peak flow value for flow events above
  a threshold equal to three times the median for for the
  entire record. MH25 is the average peak flow divided by
  the median flow for the entire record.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
mh25(qfiletempf)
}

