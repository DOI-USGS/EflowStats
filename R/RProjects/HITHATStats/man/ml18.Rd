\name{ml18}
\alias{ml18}
\title{Function to return the ML18 hydrologic indicator statistic for a given data frame}
\usage{
  ml18(qfiletempf)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ml18 numeric value of the variability in base flow for
  the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability in base
  flow. Compute the standard deviation for the ratios of
  7-day moving average flows to mean annual flows for each
  year. ML18 is the standard deviation time 100 divided by
  the mean of the ratios.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ml18(x)
}

