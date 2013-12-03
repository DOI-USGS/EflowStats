#' Function to return the l7Q10 value for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the l7Q10 value
#' 
#' @param qfiletempf data frame containing value data for one of the chosen timeseries
#' @return l7Q10 l7Q10 value for the given data frame
#' @export
#' @examples
#' data<-paste(system.file(package="NWCCompare"),"/data/qfiletempf.csv",sep="")
#' qfiletempf<-read.csv(data,stringsAsFactors=FALSE)
#' l7Q10(qfiletempf)
l7Q10 <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                              "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                              list(rollingavgs7day$year_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_90<-floor(findrank(length(sort_7day),0.90))
  if (rank_90 > 0) { 
    l7Q10<-sort_7day[rank_90]
  } else { 
    l7Q10<-NaN 
  }
  return(l7Q10)
}