#' Function to return the FL3 hydrologic indicator statistic for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the average number of events with flows below 5% of mean for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fl3 numeric containing the average number of events with flows below 5% of mean for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fl3(qfiletempf)
fl3 <- function(qfiletempf, pref = "mean") {
  lfcrit <- 0.05*mean(qfiletempf$discharge)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  lfcountbyyr <- rep(0, noyrs)
  counter <- 0
  for (i in as.character(noyears$Year[1]):as.character(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
                         i)
    echfcrit <- subset(subsetyr, subsetyr$discharge < 
                         lfcrit)
    counter <- counter + 1
    lfcountbyyr[counter] <- length(echfcrit$discharge)
  }
  lfcntbyyr <- lfcountbyyr
  if (pref == "median") {
    fl3 <- median(lfcntbyyr)
  }
  else {
    fl3 <- mean(lfcntbyyr)
  }
  return(fl3)
}