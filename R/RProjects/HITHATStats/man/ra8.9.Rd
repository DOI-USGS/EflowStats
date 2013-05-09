\name{ra8.9}
\alias{ra8.9}
\title{Function to return the RA8 and RA9 hydrologic indicator statistics for a given data frame}
\usage{
  ra8.9(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  ra8.9 list containing the variability and mean annual
  changes in flow from one day to the next for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability and mean
  annual changes in flow from one day to the next for the
  entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ra8.9(qfiletempf)
}

