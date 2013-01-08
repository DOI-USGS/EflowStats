\name{mh20}
\alias{mh20}
\title{Function to return the MH20 hydrologic indicator statistic for a given data frame}
\usage{
  mh20(x, drainarea, pref = "mean")
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}

  \item{drainarea}{numeric value of drainage area for a
  site}
}
\value{
  mh20 numeric value of the mean or median of annual
  maximum flows divided by the drainage area for the given
  data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates NH20.
  Specific mean annual maximum flow. MH20 is the mean (or
  median - use preference option) of the annual maximum
  flows divided by the drainage area.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
drainarea<-56.5
mh20(x,drainarea)
}

