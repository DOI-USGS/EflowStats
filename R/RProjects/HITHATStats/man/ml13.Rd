\name{ml13}
\alias{ml13}
\title{Function to return the ML13 hydrologic indicator statistic for a given data frame}
\usage{
  ml13(qfiletempf)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  ml13 numeric value of the variability of minimum monthly
  flow for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge","month_val" and "year_val" and
  calculates the variability across minimum monthly flow
  values. compute the mean and standard deviation for the
  minimum monthly flows over the entire record. ML13 is the
  standard deviation times 100 divided by the mean minimum
  monthly flow.
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
x<-read.csv(load_data)
ml13(x)
}

