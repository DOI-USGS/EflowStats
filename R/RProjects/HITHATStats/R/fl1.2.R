#' Function to return the FL1 and FL2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the low flood pulse count and variability in low flood pulse count for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fl1.2 list of low flood pulse count and variability for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fl1.2(qfiletempf)
fl1.2 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.75))
  lfcrit <- sortq[frank]
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
  meanfl1 <- mean(lfcntbyyr)
  stdevfl1 <- sd(lfcntbyyr)
  fl2 <- (stdevfl1 * 100)/meanfl1
  if (pref == "median") {
    fl1 <- median(lfcntbyyr)
  }
  else {
    fl1 <- mean(lfcntbyyr)
  }
  fl1.2<-list(fl1=fl1,fl2=fl2)
  return(fl1.2)
}