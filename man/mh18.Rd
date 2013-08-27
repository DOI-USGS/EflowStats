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
  mh18 numeric value of mh18 for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates MH18,
  variability across annual maximum flows. Compute the logs
  (log10) of the maximum annual flows. Find the standard
  deviation and mean for these values. MH18 is the standard
  deviation times 100 divided by the mean
  (percent-spatial).
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
mh18(x)
}

