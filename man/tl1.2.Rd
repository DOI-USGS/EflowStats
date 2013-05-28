\name{tl1.2}
\alias{tl1.2}
\title{Function to return the TL1 and TL2 hydrologic indicator statistics for a given data frame}
\usage{
  tl1.2(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  tl1.2 list containing the mean and variability of the
  Julian date of the annual minimum flow for the given data
  frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean and variability
  of the Julian date of the annual minimum flow for the
  entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
tl1.2(qfiletempf)
}

