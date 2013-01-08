\name{ma12.23}
\alias{ma12.23}
\title{Function to return the MA12-23 hydrologic indicator statistics for a given data frame}
\usage{
  ma12.23(qfiletempf, pref = "mean")
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ma12.23 data frame containing the mean or medians for
  each month
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "month_val" and calculates the
  means (or medians - use preference option) of monthly
  flow values. Compute the means for each month over the
  entire record. For examples, MA12 is the mean of all
  January flow values.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ma12.23(x)
}

