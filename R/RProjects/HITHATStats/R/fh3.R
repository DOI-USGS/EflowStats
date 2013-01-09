#' Function to return the FH3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flood pulse count (above 3 times the median) for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh3 numeric value of high flood pulse count for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh3(qfiletempf)
fh3 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 3 * ma2(qfiletempf)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh3 <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
                         i)
    echfcrit <- subset(subsetyr, subsetyr$discharge > 
                         hfcrit)
    counter <- counter + 1
    hfcountbyyrfh3[counter] <- length(echfcrit$discharge)
  }
  hfcntbyyrfh3 <- hfcountbyyrfh3
  if (pref == "median") {
    fh3 <- median(hfcntbyyrfh3)
  }
  else {
    fh3 <- mean(hfcntbyyrfh3)
  }
  return(fh3)
}