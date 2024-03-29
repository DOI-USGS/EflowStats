% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_dataToWaterYear.R
\name{cut_dataToWaterYear}
\alias{cut_dataToWaterYear}
\title{Cuts the discharge time series to full water years}
\usage{
cut_dataToWaterYear(x, yearType, wyMonth = 10L)
}
\arguments{
\item{x}{data.frame containing a vector of date values in the first 
column and vector of numeric flow values in the second column.}

\item{yearType}{A character of either "water" or "calendar" indicating 
whether to use water years or calendar years, respectively.}

\item{wyMonth}{A numeric. The month of the year in which the water year starts 
(1=January, 12=December). The water year begins on the first day of wyMonth.}
}
\value{
data.frame in original structure, but cut to full water years
}
\description{
Cuts the discharge time series to full water years
}
\details{
To ensure the input data is valid for use with other 
EflowStats functions. 
#' \enumerate{
  \item First column must be of class `Date`.
  \item Second must be of class `numeric`.
  \item `yearType` input must be either "water" or "calendar". 
  \item `wyMonth`input must be of class `integer`. 
}
}
\examples{
x <- sampleData[, c('date', 'discharge')]
cut_dataToWaterYear(x,10L)
}
