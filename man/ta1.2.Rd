\name{ta1.2}
\alias{ta1.2}
\title{Function to return the TA1 and TA2 hydrologic indicator statistics for a given data frame}
\usage{
  ta1.2(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  ta1.2 list containing the constancy and predictability
  for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the Colwell constancy
  and predictability for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ta1.2(qfiletempf)
}

