\name{FlowStatsICP}
\alias{FlowStatsICP}
\title{Function to run ICP HIT/HAT statistics for a given data set}
\usage{
FlowStatsICP(data, drain_area)
}
\arguments{
  \item{data}{data frame of daily flow data}

  \item{drain_area}{value of site drainage area}
}
\value{
Output data frame of calculated statistics
}
\description{
This function accepts a data frame of daily flow data and
returns a data frame of calculated HIT/HAT statistics
}
\examples{
drainage_url<-"http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
sites<-"02177000"
drain_url<-paste(drainage_url,sites,sep="")
drain_area<-getDrainageArea(drain_url)
qfiletempf<-sampleData
qfiletempf$date <- as.Date(qfiletempf$date,"\%m/\%d/\%y")
FlowStatsICP(qfiletempf,drain_area)
}

