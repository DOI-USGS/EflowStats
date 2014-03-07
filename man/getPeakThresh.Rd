\name{getPeakThresh}
\alias{getPeakThresh}
\title{Function to return a specified flood threshold using data from the NWISWeb peak values service}
\usage{
getPeakThresh(obs_data, peakValues, perc)
}
\arguments{
  \item{obs_data}{data frame containing processed NWIS
  daily flow data for the site}

  \item{peakValues}{data frame containing NWIS annual peak
  flow data for the site}

  \item{perc}{value containing the desired percentile to be
  calculated}
}
\value{
thresh numeric containing the 1.67-year flood threshold
}
\description{
This function accepts obs_data and peak_data data frames
that contain observed data and peak annual flow data for
the desired NWIS site and a 'perc' percentile value. It
returns a value 'thresh' calculated by: 1.67-year flood
threshold (Olden and Poff, 2003)-For indices FH11, DH22,
DH23, DH24, TA3, and TH3, compute the log10 of the peak
annual flows. Compute the log10 of the daily flows for the
peak annual flow days. Calculate the coefficients for a
linear regression equation for logs of peak annual flow
versus logs of average daily flow for peak days. Using the
log peak flow for the 1.67-year recurrence interval (60th
percentile) as input to the regression equation, predict
the log10 of the average daily flow. The threshold is 10 to
the log10 (average daily flow) power (cubic feet per
second). for the 5-year recurrence interval (80th)
percentile, used by indices TL3 and TL4, follow the same
process, inputing a different 'perc' value.
}
\examples{
qfiletempf<-sampleData
sites<-"02178400"
peakValues<-getPeakData(sites)
getPeakThresh(qfiletempf,peakValues,.6)
}

