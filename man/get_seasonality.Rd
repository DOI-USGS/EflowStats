% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_seasonality.R
\name{get_seasonality}
\alias{get_seasonality}
\title{Function to compute the seasonal factors (amplitude and phase) for a given data series}
\usage{
get_seasonality(x, yearType = "water", wyMonth = 10L)
}
\arguments{
\item{x}{A dataframe containing a vector of date values in the first column and vector of numeric flow values in the second column.}

\item{yearType}{A charcter of either "water" or "calendar" indicating whether to use water years or calendar years, respectively.}

\item{wyMonth}{A numeric. The month of the year in which the water year starts 
(1=January, 12=December). The water year begins on the first day of wyMonth.}
}
\value{
get_seasonality vector of seasonal factors (amplitude and phase)
}
\description{
This function accepts a data frame containing daily streamflow data, then computes get_seasonality 
variables by first standardizing flows, the fitting relation 
A*cos(2*pi*t) + B*sin(2*pi*t)1) Get decimal yearand returns the amplitude and phase
}
\examples{
x <- sampleData[c("date","discharge")]
get_seasonality(x=x)
}
