#' Function to return the FH5 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flood pulse count (above the median) for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh5 numeric value of high flood pulse count for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh5(qfiletempf)
fh5 <- function(qfiletempf, pref = "mean") {
  hfcrit <- ma2(qfiletempf)
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
                         hfcrit)
    counter <- counter + 1
    hfcountbyyrfh4[counter] <- length(echfcrit$discharge)
  }
  hfcntbyyrfh4 <- hfcountbyyrfh4
  if (pref == "median") {
    fh5 <- median(hfcntbyyrfh4)
  }
  else {
    fh5 <- mean(hfcntbyyrfh4)
  }
  return(fh5)
}