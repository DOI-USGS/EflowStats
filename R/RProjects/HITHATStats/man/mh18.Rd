\name{mh18}
\alias{mh18}
\title{Function to return the MH18 hydrologic indicator statistic for a given data frame}
\usage{
  mh18(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  mh18 numeric value of the standard deviation times 100
  divided by the mean for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates MH18.
  Variability across annual maximum flows. Compute the
  log10s of the maximum annual flows. Find the standard
  deviation and mean for these values. MH18 is the sd times
  100 divided by the mean.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
mh18(x)
}

