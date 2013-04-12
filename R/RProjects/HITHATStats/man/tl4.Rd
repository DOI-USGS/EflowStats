\name{tl4}
\alias{tl4}
\title{Function to return the TL4 hydrologic indicator statistic for a given data frame}
\usage{
  tl4(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  tl4 numeric containing the maximum of annual ratios of
  days above the 20th pctl to days in the year for the
  given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the predictability of
  flow above the 20th percentile for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
tl4(qfiletempf)
}

