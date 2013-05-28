\name{fh3}
\alias{fh3}
\title{Function to return the FH3 hydrologic indicator statistic for a given data frame}
\usage{
  fh3(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fh3 numeric value of high flood pulse count for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the high flood pulse
  count (above 3 times the median) for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fh3(qfiletempf)
}

