\name{mh23}
\alias{mh23}
\title{Function to return the MH23 hydrologic indicator statistic for a given data frame}
\usage{
  mh23(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  mh23 numeric value of the average volume of threshold
  event divided by median flow for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates MH23. High flow volume.
  Compute the average volume for flow events above a
  threshold equal to seven times the median flow for the
  entire record. MH23 is the average volume divided by the
  median flow for the entire record.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
mh23(x)
}

