\name{th1.2}
\alias{th1.2}
\title{Function to return the TH1 and TH2 hydrologic indicator statistics for a given data frame}
\usage{
  th1.2(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  th1.2 list containing the mean and variability of the
  Julian date of the annual maximum flow for the given data
  frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean and variability
  of the Julian date of the annual maximum flow for the
  entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
th1.2(qfiletempf)
}

