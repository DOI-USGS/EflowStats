\name{bfi}
\alias{bfi}
\title{Function to return the base flow index for a given data frame}
\usage{
  bfi(qfiletempf)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  bfi numeric value of the base flow index for the given
  data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the base flow index of
  the daily flow values for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
bfi(x)
}

