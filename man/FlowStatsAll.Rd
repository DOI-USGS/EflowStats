\name{FlowStatsAll}
\alias{FlowStatsAll}
\title{Function to run HIT/HAT statistics for a given data set}
\usage{
FlowStatsAll(data, drain_area,
  stats = "magStat,flowStat,timStat,rateStat,otherStat,durStat")
}
\arguments{
  \item{data}{data frame of daily flow data}

  \item{drain_area}{drainage area for a given site}

  \item{stats}{list of requested stat groups}
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
FlowStatsAll(qfiletempf,drain_area,"magStat,flowStat,durStat,timStat,rateStat")
}

