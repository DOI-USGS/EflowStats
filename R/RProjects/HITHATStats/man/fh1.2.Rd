\name{fh1.2}
\alias{fh1.2}
\title{Function to return the FH1 and FH2 hydrologic indicator statistics for a given data frame}
\usage{
  fh1.2(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fh1.2 list of high flood pulse count and variability for
  the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the high flood pulse
  count (above 75th percentile) and variability in high
  flood pulse count for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fh1.2(qfiletempf)
}

