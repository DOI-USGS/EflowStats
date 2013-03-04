\name{FlowStats}
\alias{FlowStats}
\title{Function to run HIT/HAT statistics for a given data set}
\usage{
  FlowStats(data, drain_area)
}
\arguments{
  \item{data}{data frame of daily flow data}

  \item{drain_area}{drainage area for a given site}
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
load_data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
qfiletempf<-read.csv(load_data,stringsAsFactors=FALSE)
FlowStats(qfiletempf,drain_area)
}

