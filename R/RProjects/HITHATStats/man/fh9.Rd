\name{fh9}
\alias{fh9}
\title{Function to return the FH9 hydrologic indicator statistic for a given data frame}
\usage{
  fh9(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fh9 numeric value of high flood pulse count for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the high flood pulse
  count (above the 75 percent exceedance value) for the
  entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fh9(qfiletempf)
}

