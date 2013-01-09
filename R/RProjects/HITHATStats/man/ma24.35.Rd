\name{ma24.35}
\alias{ma24.35}
\title{Function to return the MA24-35 hydrologic indicator statistics for a given data frame}
\usage{
  ma24.35(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  ma234.35 data frame containing the MA24-MA35 statistics
}
\description{
  This function accepts a data frame that contains columns
  named "discharge", "year_val" and "month_val" and
  calculates the variability of monthly flow values.
  compute the standard deviation for each month in each
  year. Divide the sd by the mean for each month. Mean (or
  median - use preference option) these values for each
  month across years.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
ma24.35(qfiletempf)
}

