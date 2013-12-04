\name{dl14}
\alias{dl14}
\title{Function to return the DL14 hydrologic indicator statistic for a given data frame}
\usage{
  dl14(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl14 numeric containing the 75 percent exceedance value
  divided by the median for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the 75 percent
  exceedance value divided by the median for the entire
  record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl14(qfiletempf)
}

