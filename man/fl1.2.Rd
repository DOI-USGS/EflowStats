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
  fl1.2 list of FL1 and FL2 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates FL1; Low flood pulse
  count. Compute the average number of flow events with
  flows below a threshold equal to the 25th percentile
  value for the entire flow record. FL1 is the average (or
  median-Use Preference option) number of events (number of
  events/year-temporal). and FL2; Variability in low pulse
  count. Compute the standard deviation in the annual pulse
  counts for FL1. FL2 is 100 times the standard deviation
  divided by the mean pulse count (percent-spatial).
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fl1.2(qfiletempf)
}

