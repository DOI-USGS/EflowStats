\name{rmse}
\alias{rmse}
\title{Function to return the root mean square error between two data series}
\usage{
  rmse(timeseries1, timeseries2)
}
\arguments{
  \item{timeseries1}{data frame containing value data for
  one of the chosen timeseries}

  \item{timeseries2}{data frame continaing value data for
  the second chosen timeseries}
}
\value{
  rmse root mean square error value between the two
  timeseries
}
\description{
  This function accepts two data frames containing daily
  data series and returns the root mean square error
}
\examples{
obs_data<-paste(system.file(package="NWCCompare"),"/data/obs_data.csv",sep="")
mod_data<-paste(system.file(package="NWCCompare"),"/data/mod_data.csv",sep="")
timeseries1<-read.csv(obs_data)$discharge
timeseries2<-read.csv(mod_data)$discharge
rmse(timeseries1,timeseries2)
}

