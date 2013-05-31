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
  fh9 numeric value of FH9 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates FH9; Flood frequency.
  Compute the average number of flow events with flows
  above a threshold equal to 75-percent exceedence value
  for the entire flow record. FH9 is the average (or
  median-Use Preference option) number of events (number of
  events/year-temporal).
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fh9(qfiletempf)
}

