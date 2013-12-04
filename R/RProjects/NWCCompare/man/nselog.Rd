\name{nselog}
\alias{nselog}
\title{Function to return the Nash-Sutcliffe value between the natural logarithms of the two data series}
\usage{
  nselog(timeseries1, timeseries2)
}
\arguments{
  \item{timeseries1}{data frame containing value data for
  one of the chosen timeseries}

  \item{timeseries2}{data frame continaing value data for
  the second chosen timeseries}
}
\value{
  nselog Nash-Sutcliffe value between the natural log of
  the two timeseries
}
\description{
  This function accepts two data frames containing daily
  data series and returns the Nash-Sutcliffe value of the
  natural logarithms of the data, with zeros removed.
}
\examples{
obs_data<-paste(system.file(package="NWCCompare"),"/data/obs_data.csv",sep="")
mod_data<-paste(system.file(package="NWCCompare"),"/data/mod_data.csv",sep="")
timeseries1<-read.csv(obs_data)$discharge
timeseries2<-read.csv(mod_data)$discharge
nselog(timeseries1,timeseries2)
}

