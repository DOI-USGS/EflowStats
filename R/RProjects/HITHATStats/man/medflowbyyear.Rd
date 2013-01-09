\name{medflowbyyear}
\alias{medflowbyyear}
\title{Function to return the median flow by year for a given data frame}
\usage{
  medflowbyyear(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  medflowbyyear numeric value of the median flow by year
  for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates the
  median flow by year for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
medflowbyyear(qfiletempf)
}

