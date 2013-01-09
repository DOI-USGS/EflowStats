#' Function to return the FH1 and FH2 hydrologic indicator statistics for a given data frame
#' 
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the high flood pulse count (above 75th percentile) and variability in high flood pulse count for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh1.2 list of high flood pulse count and variability for the given data frame
#' @export
#' @examples
#' load_data<-paste(system.file(package="HITHATStats"),"/data/obs_data.csv",sep="")
#' qfiletempf<-read.csv(load_data)
#' fh1.2(qfiletempf)
fh1.2 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.25))
  hfcrit <- sortq[frank]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyr <- rep(0, noyrs)
  counter <- 0
  for (i in as.numeric(noyears$Year[1]):as.numeric(noyears$Year[noyrs])) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$year_val) == 
                         i)
    echfcrit <- subset(subsetyr, subsetyr$discharge > 
                         hfcrit)
    counter <- counter + 1
    hfcountbyyr[counter] <- length(echfcrit$discharge)
  }
  hfcntbyyr <- hfcountbyyr
  meanfh1<-mean(hfcntbyyr)
  stdevfh1<-sd(hfcntbyyr)
  fh2<-(stdevfh1*100)/meanfh1
  if (pref == "median") {
    fh1 <- median(hfcntbyyr)
  }
  else {
    fh1 <- mean(hfcntbyyr)
  }
  fh1.2<-list(fh1=fh1,fh2=fh2)
  return(fh1.2)
}