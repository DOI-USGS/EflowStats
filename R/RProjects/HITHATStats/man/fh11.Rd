\name{fh11}
\alias{fh11}
\title{Function to return the FH11 hydrologic indicator statistic for a given data frame}
\usage{
  fh11(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  fh11 numeric containing the median number of flow evens
  above the 60th percentile for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the mean number of flow
  events with flows above the 60th percentile for the
  entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fh11(qfiletempf)
}

