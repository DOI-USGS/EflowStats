\name{fl1.2}
\alias{fl1.2}
\title{Function to return the FL1 and FL2 hydrologic indicator statistics for a given data frame}
\usage{
  fl1.2(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fl1.2 list of low flood pulse count and variability for
  the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the low flood pulse
  count and variability in low flood pulse count for the
  entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fl1.2(qfiletempf)
}

