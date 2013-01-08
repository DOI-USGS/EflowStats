\name{ml1.12}
\alias{ml1.12}
\title{Function to return the ML1-ML12 hydrologic indicator statistics for a given data frame}
\usage{
  ml1.12(qfiletemp)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ml1.12 data frame containing the mean or medians for each
  month
}
\description{
  This function accepts a data frame that contains columns
  named "discharge", "year_val" and "month_val" and
  calculates the means (or medians - use preference option)
  minimum flows for each month across all years. Compute
  the minimums for each month. For example, ML1 is the mean
  of the minimums of all January flow values.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ml1.12(x)
}

