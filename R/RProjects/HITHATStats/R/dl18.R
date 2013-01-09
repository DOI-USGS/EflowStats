#' Function to return the DL18 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of the annual number of zero-flow days for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl18 numeric containing the mean of the annual number of zero-flow days for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' dl18(qfiletempf)
dl18 <- function(qfiletempf, pref = "mean") {
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountzeros <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, qfiletempf$year_val == 
                         i)
    echfcrit <- subset(subsetyr, subsetyr$discharge < 
                         0.001)
    counter <- counter + 1
    hfcountzeros[counter] <- length(echfcrit$discharge)
  }
  hfcntzeros <- hfcountzeros
  if (pref == "median") {
    dl18 <- median(hfcntzeros)
  }
  else {
    dl18 <- mean(hfcntzeros)
  }
  return(dl18)
}