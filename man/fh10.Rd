\name{fh10}
\alias{fh10}
\title{Function to return the FH10 hydrologic indicator statistic for a given data frame}
\usage{
  fh10(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fh10 numeric value of average annual flow events above a
  threshold for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the average number of
  annual flow events above a threshold (the median of the
  annual minima) for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fh10(qfiletempf)
}

