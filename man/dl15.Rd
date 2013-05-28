\name{dl15}
\alias{dl15}
\title{Function to return the DL15 hydrologic indicator statistic for a given data frame}
\usage{
  dl15(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl15 numeric containing the 90 percent exceedance value
  divided by the median for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the 90 percent
  exceedance value divided by the median for the entire
  record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl15(qfiletempf)
}

