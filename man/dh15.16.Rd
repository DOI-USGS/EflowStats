\name{dh15.16}
\alias{dh15.16}
\title{Function to return the DH15 and DH16 hydrologic indicator statistics for a given data frame}
\usage{
  dh15.16(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dh15.16 list containing the median duration of high pulse
  flow and variability for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the variability and
  median average pulse duration for each year of flow above
  the 75th percentile for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data, stringsAsFactors=FALSE)
dh15.16(qfiletempf)
}

