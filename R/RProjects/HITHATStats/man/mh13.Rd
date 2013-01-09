\name{mh13}
\alias{mh13}
\title{Function to return the MH13 hydrologic indicator statistic for a given data frame}
\usage{
  mh13(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
  mh13 numeric value of the standard deviation times 100
  divided by the mean maximum monthly flow for the given
  data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge", "year_val" and "month_val" and
  calculates MH13. Variability across maximum monthly flow
  values. Compute the mean and standard deviation for the
  maximum monthly flows over the entire flow record. MH13
  is the standard deviation times 100 divided by the mean
  maximum monthly flow for all years.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
mh13(qfiletempf)
}

