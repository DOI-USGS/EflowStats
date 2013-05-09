\name{getPeakData}
\alias{getPeakData}
\title{Function to return data from the NWISWeb peak values service}
\usage{
  getPeakData(obs_data, peakDaily)

  getPeakData(obs_data, peakDaily)
}
\arguments{
  \item{sites}{string containing the site for the
  retrieval}

  \item{obs_data}{data frame containing processed NWIS
  daily flow data for the site}

  \item{peakDaily}{data frame containing NWIS annual peak
  flow data for the site}
}
\value{
  Peak a data frame containing columns 'Date' and
  'Discharge'

  thresh numeric containing the 1.67-year flood threshold
}
\description{
  This function accepts a sites parameter that contains the
  desired NWIS site. It returns a data frame containing
  "Date" and "Discharge"

  This function accepts obs_data and peak_data data frames
  that contain observed data and peak annual flow data for
  the desired NWIS site. It returns a value 'thresh' that
  is the 1.67-year flood threshold for the site
}
\examples{
sites<-"02178400"
getPeakData(sites)
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
obs_data<-read.csv(load_data,stringsAsFactors=FALSE)
sites<-"02178400"
peakDaily<-getPeakData(sites)
getPeakThresh(obs_data,peakDaily)
}

