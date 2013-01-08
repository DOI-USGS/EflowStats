\name{ma3}
\alias{ma3}
\title{Function to return the MA3 hydrologic indicator statistic for a given data frame}
\usage{
  ma3(qfiletempf, pref = "mean")
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ma3 numeric value of the mean or median of the cvs for
  each year for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean (or median -
  use preference option) of the coefficients of variation
  (standard deviation/mean) for each year. Compute the
  coefficent of variation for each year of daily flows.
  Compute the mean/median of these.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ma3(x)
}

