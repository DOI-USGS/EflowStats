\name{getPeakThresh}
\alias{getPeakThresh}
\title{Function to return the 1.67-year flood threshold using data from the NWISWeb peak values service}
\usage{
  getPeakThresh(obs_data, peakValues)
}
\arguments{
  \item{obs_data}{data frame containing processed NWIS
  daily flow data for the site}

  \item{peakDaily}{data frame containing NWIS annual peak
  flow data for the site}
}
\value{
  thresh numeric containing the 1.67-year flood threshold
}
\description{
  This function accepts obs_data and peak_data data frames
  that contain observed data and peak annual flow data for
  the desired NWIS site. It returns a value 'thresh' that
  is the 1.67-year flood threshold for the site
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
obs_data<-read.csv(load_data,stringsAsFactors=FALSE)
sites<-"02178400"
peakValues<-getPeakData(sites)
getPeakThresh(obs_data,peakValues)
}

