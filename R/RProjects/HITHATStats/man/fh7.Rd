\name{fh7}
\alias{fh7}
\title{Function to return the FH7 hydrologic indicator statistic for a given data frame}
\usage{
  fh7(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fh7 numeric value of high flood pulse count for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the high flood pulse
  count (above 7 times the median) for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fh7(qfiletempf)
}

