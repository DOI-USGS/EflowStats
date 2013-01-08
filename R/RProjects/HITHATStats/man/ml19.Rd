\name{ml19}
\alias{ml19}
\title{Function to return the ML19 hydrologic indicator statistic for a given data frame}
\usage{
  ml19(qfiletempf, pref = "mean")
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ml19 numeric value of the min annual flow/mean annual
  flow for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates the base
  flow. Compute the ratios of the minimum annual flow to
  mean annual flow for each year. ML19 is the mean (or
  median - use preference option) of these ratios times
  100.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ml19(x)
}

