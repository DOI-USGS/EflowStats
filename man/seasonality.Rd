\name{seasonality}
\alias{seasonality}
\title{Function to compute the seasonal factors (amplitude and phase) for a given data series}
\usage{
seasonality(timeseries)
}
\arguments{
  \item{timeseries}{data frame containing daily discharge
  data}
}
\value{
seasonalityv vector of seasonal factors (amplitude and
phase)
}
\description{
This function accepts a data frame containing daily
streamflow data, then computes seasonality variables by
first standardizing flows, the fitting relation
A*cos(2*pi*t) + B*sin(2*pi*t)1) Get decimal yearand returns
the amplitude and phase
}

