#' Function to return the FH10 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the average number of annual flow events above a threshold (the median of the annual minima) for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh10 numeric value of average annual flow events above a threshold for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh10(qfiletempf)
fh10 <- function(qfiletempf, pref = "mean") {
  minbyyear <- aggregate(qfiletempf$discharge,list(qfiletempf$year_val),FUN=min,na.rm=TRUE)
  medmin <- median(minbyyear$x)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh4 <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
                         i)
    echfcrit <- subset(subsetyr, subsetyr$discharge > 
                         medmin)
    counter <- counter + 1
    hfcountbyyrfh4[counter] <- length(echfcrit$discharge)
  }
  hfcntbyyrfh4 <- hfcountbyyrfh4
  if (pref == "median") {
    fh10 <- median(hfcntbyyrfh4)
  }
  else {
    fh10 <- mean(hfcntbyyrfh4)
  }
  return(fh10)
}