\name{dh19}
\alias{dh19}
\title{Function to return the DH19 hydrologic indicator statistic for a given data frame}
\usage{
  dh19(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh19 numeric containing DH19 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates DH19; High flow
  duration. Compute the average duration of flow events
  with flows above a threshold equal to seven times the
  median flow value for the entire flow record. DH19 is the
  average (or median-Use Preference option) duration of the
  events (days-temporal).
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dh19(qfiletempf)
}

