\name{tl4}
\alias{tl4}
\title{Function to return the TL4 hydrologic indicator statistic for a given data frame}
\usage{
  tl4(qfiletempf, thresh)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{thresh}{value containing the 5-year recurrence
  value for the site}
}
\value{
  tl4 numeric containing TL4 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and a threshold value obtained using
  the peakdata and getPeakThresh functions and calculates
  TL4; Seasonal predictability of non-low flow. Compute the
  number of days that flow is above the 5-year flood
  threshold as the ratio of number of days to 365 or 366
  (leap year) for each year. TL4 is the maximum of the
  yearly ratios (dimensionless-spatial).
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
tl4(qfiletempf, 1161.38)
}

