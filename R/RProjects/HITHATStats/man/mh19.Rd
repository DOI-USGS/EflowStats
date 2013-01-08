\name{mh19}
\alias{mh19}
\title{Function to return the MH19 hydrologic indicator statistic for a given data frame}
\usage{
  mh19(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  mh19 numeric value of the mean daily flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates the
  skewness in annual maximum flows.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
mh19(x)
}

