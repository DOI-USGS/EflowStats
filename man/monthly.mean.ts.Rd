\name{monthly.mean.ts}
\alias{monthly.mean.ts}
\title{Function to return the monthly mean time series for a given daily data series}
\usage{
  monthly.mean.ts(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing daily value data}
}
\value{
  meanmonts data frame containing mean monthly values
}
\description{
  This function accepts a data frame containing daily data
  and returns a data frame containing mean monthly data
}
\examples{
qfiletempf<-sampleData
monthly.mean.ts(qfiletempf)
}

