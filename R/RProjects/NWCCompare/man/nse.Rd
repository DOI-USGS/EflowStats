\name{nse}
\alias{nse}
\title{Function to return the Nash-Sutcliffe value between two data series}
\usage{
  nse(timeseries1, timeseries2)
}
\arguments{
  \item{timeseries1}{data frame containing value data for
  one of the chosen timeseries}

  \item{timeseries2}{data frame continaing value data for
  the second chosen timeseries}
}
\value{
  nse Nash-Sutcliffe value between the two timeseries
}
\description{
  This function accepts two data frames containing daily
  data series and returns the Nash-Sutcliffe value
}
\examples{
obs_data<-paste(system.file(package="NWCCompare"),"/data/obs_data.csv",sep="")
mod_data<-paste(system.file(package="NWCCompare"),"/data/mod_data.csv",sep="")
timeseries1<-read.csv(obs_data)$discharge
timeseries2<-read.csv(mod_data)$discharge
nse(timeseries1,timeseries2)
}

