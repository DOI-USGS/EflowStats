\name{SWE_CSV_IHA}
\alias{SWE_CSV_IHA}
\title{Function to return NWC Intercomparison portal modeled data for a given site}
\usage{
  SWE_CSV_IHA(input)
}
\arguments{
  \item{input}{url for SOS service for desired site data}
}
\value{
  flow data frame containing desired time series
}
\description{
  This function accepts a url and returns a data frame of
  daily data for that SOS request
}
\examples{
url <- "http://cida.usgs.gov/nwc/thredds/sos/watersmart/stats/stats-SE-DENSE1-2.03.nc?request=GetObservation&service=SOS&version=1.0.0&offering=02178400&observedProperty=Streamflow"
SWE_CSV_IHA(url)
}

