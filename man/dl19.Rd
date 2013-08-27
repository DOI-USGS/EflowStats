\name{dl19}
\alias{dl19}
\title{Function to return the DL19 hydrologic indicator statistic for a given data frame}
\usage{
  dl19(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  dl19 numeric containing DL19 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates DL19; Variability in the
  number of zero-flow days. Compute the standard deviation
  for the annual number of zero-flow days. DL19 is 100
  times the standard deviation divided by the mean annual
  number of zero-flow days (percent-spatial).
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl19(qfiletempf)
}

