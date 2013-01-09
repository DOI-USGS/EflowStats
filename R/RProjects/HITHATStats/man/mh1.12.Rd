\name{mh1.12}
\alias{mh1.12}
\title{Function to return the MH1-MH12 hydrologic indicator statistics for a given data frame}
\usage{
  mh1.12(qfiletemp)
}
\arguments{
  \item{qfiletemp}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  mh1.12 data frame containing the mean or medians for each
  month
}
\description{
  This function accepts a data frame that contains columns
  named "discharge", "year_val" and "month_val" and
  calculates the mean (or median - use preference option)
  maximum flows for each month across all years. Compute
  the maximums for each month. For example, MH1 is the mean
  of the maximums of all January flow values.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletemp<-read.csv(load_data)
mh1.12(qfiletemp)
}

