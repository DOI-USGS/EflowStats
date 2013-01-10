\name{fl3}
\alias{fl3}
\title{Function to return the FL3 hydrologic indicator statistic for a given data frame}
\usage{
  fl3(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  fl3 numeric containing the average number of events with
  flows below 5% of mean for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates the average number of
  events with flows below 5% of mean for the entire record
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
fl3(qfiletempf)
}

