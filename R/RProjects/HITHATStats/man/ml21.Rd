\name{ml21}
\alias{ml21}
\title{Function to return the ML21 hydrologic indicator statistic for a given data frame}
\usage{
  ml21(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ml21 numeric value of the standard deviation times 100
  divided by the mean for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates ML21.
  Variability across annual minimum flows. Compute the mean
  and standard deviation for the annual minimum flows. ML21
  is the standard deviation times 100 divided by the mean.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ml21(x)
}

