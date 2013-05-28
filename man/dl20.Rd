\name{dl20}
\alias{dl20}
\title{Function to return the DL20 hydrologic indicator statistic for a given data frame}
\usage{
  dl20(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl20 numeric containing the number of zero-flow months
  for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the number of zero-flow
  months for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl20(qfiletempf)
}

