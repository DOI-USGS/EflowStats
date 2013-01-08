\name{ml17}
\alias{ml17}
\title{Function to return the ML17 hydrologic indicator statistic for a given data frame}
\usage{
  ml17(qfiletempf, pref = "mean")
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ml17 numeric value of the mean annual flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the base flow. Compute
  the mean annual flows. Compute the minimum of a 7-day
  moving average flow for each year and divide them by the
  mean annual flow for that year. ML17 is the mean (or
  median - use preference option) of ratios.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ml17(x)
}

