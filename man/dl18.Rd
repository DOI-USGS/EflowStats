\name{dl18}
\alias{dl18}
\title{Function to return the DL18 hydrologic indicator statistic for a given data frame}
\usage{
  dl18(qfiletempf, pref = "mean")
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}

  \item{pref}{string containing a "mean" or "median"
  preference}
}
\value{
  dl18 numeric containing DL18 for the given data frame
}
\description{
  This function accepts a data frame that contains a column
  named "discharge" and calculates DL18; Number of
  zero-flow days. Count the number of zero-flow days for
  the entire flow record. DL18 is the mean (or median-Use
  Preference option) annual number of zero flow days
  (number of days/year-temporal).
}
\examples{
load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
qfiletempf<-read.csv(load_data)
dl18(qfiletempf)
}

