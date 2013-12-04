\name{mh19}
\alias{mh19}
\title{Function to return the MH19 hydrologic indicator statistic for a given data frame}
\usage{
  mh19(x)
}
\arguments{
  \item{x}{data frame containing a "discharge" column
  containing daily flow values}
}
\value{
  mh19 numeric value of MH19 for the given data frame
}
\description{
  This function accepts a data frame that contains columns
  named "discharge" and "year_val" and calculates the
  skewness in annual maximum flows (dimensionless-spatial).
  Use the equation: MH19 numerator = N2 ? sum(qm3)-3N ?
  sum(qm) ? sum(qm2) + 2 ? (sum(qm))3 denominator = N ?
  (N-1) ? (N-2) ? stddev3 Where: N = Number of years qm =
  Log10 (annual maximum flows) stddev = Standard deviation
  of the annual maximum flows
}
\examples{
qfiletempf<-sampleData
mh19(qfiletempf)
}

