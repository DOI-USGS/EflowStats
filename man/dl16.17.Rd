\name{dl16.17}
\alias{dl16.17}
\title{Function to return the DL16 and DL17 hydrologic indicator statistics for a given data frame}
\usage{
dl16.17(qfiletempf)
}
\arguments{
  \item{qfiletempf}{data frame containing a "discharge"
  column containing daily flow values}
}
\value{
dl16.17 list containing DL16 and DL17 for the given data
frame
}
\description{
This function accepts a data frame that contains a column
named "discharge" and calculates DL16; Low flow pulse
duration. Compute the average pulse duration for each year
for flow events below a threshold equal to the 25th
percentile value for the entire flow record. DL16 is the
median of the yearly average durations (number of
days-temporal). and DL17; Variability in low pulse
duration. Compute the standard deviation for the yearly
average low pulse durations. DL17 is 100 times the standard
deviation divided by the mean of the yearly average low
pulse durations (percent-spatial).
}
\examples{
qfiletempf<-sampleData
dl16.17(qfiletempf)
}

