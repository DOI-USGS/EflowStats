\name{mh15.17}
\alias{mh15.17}
\title{Function to return the MH15-MH17 hydrologic indicator statistic for a given data frame}
\usage{
  mh15.17(qfiletempf)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  mh15.17 list of numeric value of the 1%, 10% and 25%
  exceedence values divided by median flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates MH15. High flow
  discharge index. compute the 1% exceedence value for the
  entire data record. MH15 is the 1% exceedence value
  divided by the median flow for the entire record. MH16 is
  the 10% exceedence value divided by the median flow for
  the entire record. MH17 is the 25% exceedence value
  divided by the median flow for the entire record.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
mh15.17(x)
}

