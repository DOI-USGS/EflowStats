\name{fh11}
\alias{fh11}
\title{Function to return the FH11 hydrologic indicator statistic for a given data frame}
\usage{
  fh11(qfiletempf, thresh, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{thresh}{numeric containing 1.67-year flood
  threshold calculated by getPeakThresh}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fh11 numeric containing FH11 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and a threshold value calculated for
  the site using the peakdata and peakthresh functions and
  calculates FH11; Flood frequency. Compute the average
  number of flow events with flows above a threshold equal
  to flow corresponding to a 1.67-year recurrence interval.
  FH11 is the average (or median-Use Preference option)
  number of events (number of events/year-temporal).
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
sites<-"02178400"
peakValues<-getPeakData(sites)
thresh<-1158.495
fh11(qfiletempf,thresh)
}

