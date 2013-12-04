\name{getScenarioSites}
\alias{getScenarioSites}
\title{Function to return NWC Intercomparison portal sites with modeled data for a given model}
\usage{
  getScenarioSites(scenario_url)
}
\arguments{
  \item{scenario_url}{url for SOS service for desired model
  data}
}
\value{
  getcap list of sites with data for the chosen model
}
\description{
  This function accepts a url and returns a list of sites
  with modeled data
}
\examples{
scenario_url <- "http://cida.usgs.gov/nwc/thredds/sos/watersmart/stats/stats-SE-DENSE1-2.03.nc?request=GetCapabilities&service=SOS&version=1.0.0"
getScenarioSites(scenario_url)
}

