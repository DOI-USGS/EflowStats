\name{mh14}
\alias{mh14}
\title{Function to return the MH14 hydrologic indicator statistic for a given data frame}
\usage{
  mh14(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  mh14 numeric value of the ratio of annual maximum flow to
  median annual flow for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge", "year_val" and "month_val" and
  calculates MH14. Median of annual maximum flows. Compute
  the annual maximum flows from monthly maximum flows.
  Compute the ratio of annual maximum flow to median annual
  flow for each year. MH14 is the median of these ratios.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
mh14(qfiletempf)
}

