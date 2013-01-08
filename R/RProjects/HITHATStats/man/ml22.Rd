\name{ml22}
\alias{ml22}
\title{Function to return the ML22 hydrologic indicator statistic for a given data frame}
\usage{
  ml22(x, drainarea, pref = "mean")
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}

  \item{drainarea}{numeric value of the drainage area for a
  given site}
}
\value{
  ml22 numeric value of the mean of annual minimum flows
  divided by the drainage area for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and a numeric drainage
  area and calculates ML22. Specific mean annual minimum
  flow. ML22 is the mean (or median - use preference
  option) of the annual minimum flows divided by the
  drainage area.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
drainarea<-56.5
ml22(x,drainarea)
}

